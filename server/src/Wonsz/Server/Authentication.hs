{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Wonsz.Server.Authentication
    ( AuthenticatedUser
    , getAuthenticatedUserId
    , getAuthenticatedUserName

    , AuthToken
    , rawToken
    , authApi
    , AuthApi

    , LoginRequest(..)
    , ChangePasswordRequest(..)

    , Protected
    , protected 
    ) where

import Servant (JSON, ReqBody, Get, Post, (:>), (:<|>)(..), err401, err403, ServerError, ServerT, Handler)
import Servant.Auth.Server (FromJWT, ToJWT, JWTSettings, AuthResult(..), Auth, makeJWT, ThrowAll(..))
import Data.Aeson (FromJSON(..), ToJSON(..))

import GHC.Generics (Generic)

import Data.Time (NominalDiffTime, getCurrentTime, addUTCTime)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.UTF8 (toString, fromString)

import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Wonsz.Users.Domain as D
import Wonsz.Users

import Data.Text as Text

data AuthenticatedUser = AuthenticatedUser
    { auId :: !Int
    , name :: !String
    } 
    deriving stock (Show, Eq, Generic, Read)
    deriving anyclass (FromJSON, ToJSON, FromJWT, ToJWT)

-- optics?
getAuthenticatedUserId :: AuthenticatedUser -> Int
getAuthenticatedUserId = auId

getAuthenticatedUserName :: AuthenticatedUser -> String
getAuthenticatedUserName = name

instance ToJSON AuthToken where
  toJSON = toJSON . toString . unAuthToken 

instance FromJSON AuthToken where
  parseJSON val = AuthToken . fromString <$> parseJSON val

data LoginRequest = LoginRequest
    { username :: !String
    , password :: !String
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ChangePasswordRequest = ChangePasswordRequest 
    { newPassword :: !String 
    , currentPassword :: !String
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype AuthToken = AuthToken { unAuthToken :: ByteString }

rawToken :: AuthToken -> ByteString
rawToken = unAuthToken

type Protected auth = Auth auth AuthenticatedUser

type LoginApi = "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] AuthToken

type SessionApi auth = 
    Protected auth :> "renewToken" :> Post '[JSON] AuthToken
    :<|> Protected auth :> "changePassword" :> ReqBody '[JSON] ChangePasswordRequest :> Post '[JSON] ()

type AuthApi auth = SessionApi auth :<|> LoginApi

authApi 
    :: MonadIO m
    => UserMonad m
    => MonadError ServerError m
    => JWTSettings 
    -> ServerT (AuthApi auth) m
authApi jwt = sessionApi tokenCreator :<|> loginHandler tokenCreator 
  where
    tokenCreator = createJWT jwt

bullshitCrypto :: D.HashingAlgorithm
bullshitCrypto = D.HashingAlgorithm id

loginHandler
    :: UserMonad m
    => MonadError ServerError m
    => AuthTokenCreator m
    -> LoginRequest
    -> m AuthToken
loginHandler createToken LoginRequest{..} =  do
    user <- login bullshitCrypto $ LoginCommand (Text.pack username) (Text.pack password)
    token <- case mkAuthenticatedUser <$> user of
        Nothing -> throwError err401
        Just u -> createToken u
    case token of
        Nothing -> throwError err401
        Just t -> return t
  where
    mkAuthenticatedUser UserDescription{..} = AuthenticatedUser userId (Text.unpack userName)

protected
    :: MonadError ServerError m
    => (AuthenticatedUser -> m a)
    -> AuthResult AuthenticatedUser
    -> m a
protected action (Authenticated user) = action user
protected _ _ = throwError err401

sessionApi 
    :: UserMonad m 
    => MonadError ServerError m
    => AuthTokenCreator m
    -> ServerT (SessionApi auth) m
sessionApi tokenCreator = protected (renewToken tokenCreator) :<|> tmpChangePasswordHandler 

renewToken 
    :: MonadError ServerError m
    => AuthTokenCreator m
    -> AuthenticatedUser 
    -> m AuthToken
renewToken createToken user = 
    createToken user >>= \case
        Nothing -> throwError err401
        Just token -> return token

tmpChangePasswordHandler 
    :: UserMonad m 
    => MonadError ServerError m
    => AuthResult AuthenticatedUser
    -> ChangePasswordRequest
    -> m ()
tmpChangePasswordHandler (Authenticated user) = changePasswordHandler user 
tmpChangePasswordHandler _ = const $ throwError err401

changePasswordHandler
    :: UserMonad m 
    => MonadError ServerError m
    => AuthenticatedUser 
    -> ChangePasswordRequest
    -> m ()
changePasswordHandler user ChangePasswordRequest{..} = changePassword bullshitCrypto command
  where 
    userId = auId user
    command = ChangePasswordCommand userId userId (Text.pack newPassword)

type AuthTokenCreator m = AuthenticatedUser -> m (Maybe AuthToken)

createJWT 
     :: MonadIO m 
     => JWTSettings 
     -> AuthTokenCreator m
createJWT jwt user = liftIO $ do
     time <- addUTCTime (3600 :: NominalDiffTime) <$> getCurrentTime
     token <- fmap (AuthToken . toStrict) <$> makeJWT user jwt (Just time)
     case token of
         Left _ -> return Nothing
         Right t -> return $ Just t
