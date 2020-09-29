{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE OverloadedStrings #-}

module Wonsz.Server.Authentication
    ( AuthenticatedUser
    , getAuthenticatedUserId
    , getAuthenticatedUserName

    , AuthToken
    , rawToken
    , authApi
    , AuthApi

    , LoginRequest(..)

    , protected 
    ) where

import Servant (JSON, ReqBody, Get, Post, (:>), (:<|>)(..), err401, err403, ServerError, ServerT, Handler)
import Servant.Auth.Server (FromJWT, ToJWT, JWTSettings, AuthResult(..), Auth, makeJWT)
import Data.Aeson (FromJSON(..), ToJSON(..))

import GHC.Generics (Generic)

import Data.Time (NominalDiffTime, getCurrentTime, addUTCTime)
import Data.ByteString.Lazy.Internal as BS
import Data.ByteString.Lazy.UTF8 (toString, fromString)

import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Wonsz.Users.Domain as D
import Wonsz.Users
import Wonsz.Named

import Data.Text as Text

data AuthenticatedUser = AuthenticatedUser
    { auId :: !Int
    , auName :: !String
    } deriving (Show, Eq, Generic, Read)

-- optics?
getAuthenticatedUserId :: AuthenticatedUser -> Int
getAuthenticatedUserId = auId

getAuthenticatedUserName :: AuthenticatedUser -> String
getAuthenticatedUserName = auName

instance FromJSON AuthenticatedUser
instance ToJSON AuthenticatedUser
instance FromJWT AuthenticatedUser 
instance ToJWT AuthenticatedUser

instance ToJSON AuthToken where
  toJSON = toJSON . toString . unAuthToken 

instance FromJSON AuthToken where
  parseJSON val = AuthToken . fromString <$> parseJSON val

data LoginRequest = LoginRequest
    { username :: !String
    , password :: !String
    } deriving (Show, Eq, Generic)

instance FromJSON LoginRequest
instance ToJSON LoginRequest

newtype AuthToken = AuthToken { unAuthToken :: BS.ByteString } 

rawToken :: AuthToken -> BS.ByteString
rawToken = unAuthToken

type LoginApi = "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] AuthToken
type SessionApi = 
    "renewToken" :> Post '[JSON] AuthToken
    :<|> "changePassword" :> Get '[JSON] ()

type AuthApi auth = (Auth auth AuthenticatedUser :> SessionApi) :<|> LoginApi

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
    token <- case user of
        Nothing -> throwError err401
        Just u -> createToken u
    case token of
        Nothing -> throwError err401
        Just t -> return t

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
    -> AuthResult AuthenticatedUser
    -> ServerT SessionApi m
sessionApi tokenCreator (Authenticated user) = renewToken tokenCreator user :<|> changePasswordHandler user
sessionApi _ _ = throwError err401 :<|> throwError err401

renewToken 
    :: MonadError ServerError m
    => AuthTokenCreator m
    -> AuthenticatedUser 
    -> m AuthToken
renewToken createToken user = 
    createToken userDescription >>= \case
        Nothing -> throwError err401
        Just token -> return token
  where
    userDescription = UserDescription (getAuthenticatedUserId user) ((Text.pack . getAuthenticatedUserName) user)

changePasswordHandler
    :: UserMonad m 
    => MonadError ServerError m
    => AuthenticatedUser 
    -> m ()
changePasswordHandler user = changePassword bullshitCrypto command
  where 
    userId = auId user
    command = ChangePasswordCommand userId userId "newPassword"

renewTokenHandler 
    :: MonadError ServerError m
    => AuthTokenCreator m
    -> AuthResult AuthenticatedUser
    -> m AuthToken
renewTokenHandler = protected . renewToken 

type AuthTokenCreator m = UserDescription -> m (Maybe AuthToken)

createJWT 
     :: MonadIO m 
     => JWTSettings 
     -> AuthTokenCreator m
createJWT jwt UserDescription{..} = liftIO $ do
     time <- addUTCTime (3600 :: NominalDiffTime) <$> getCurrentTime
     token <- fmap AuthToken <$> makeJWT (AuthenticatedUser userId (Text.unpack userName)) jwt (Just time)
     case token of
         Left _ -> return Nothing
         Right t -> return $ Just t
