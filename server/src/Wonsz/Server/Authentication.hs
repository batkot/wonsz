{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Wonsz.Server.Authentication
    ( AuthenticatedUser
    , getAuthenticatedUserId
    , getAuthenticatedUserName

    , AuthToken
    , authApi
    , AuthApi
    ) where

import Servant (JSON, ReqBody, Get, Post, (:>), (:<|>)(..), err401, err403, ServerError, ServerT, Handler)
import Servant.Auth.Server (FromJWT, ToJWT, JWTSettings, AuthResult(..), Auth, makeJWT)
import Data.Aeson (FromJSON, ToJSON(..))

import GHC.Generics (Generic)

import Data.Time (NominalDiffTime, getCurrentTime, addUTCTime)
import Data.ByteString.Lazy.Internal as BS
import Data.ByteString.Lazy.UTF8 (toString)

import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Wonsz.Users

data AuthenticatedUser = AuthenticatedUser
    { auId :: !Int
    , name :: !String
    } deriving (Show, Eq, Generic, Read)

-- optics?
getAuthenticatedUserId :: AuthenticatedUser -> Int
getAuthenticatedUserId = auId

getAuthenticatedUserName :: AuthenticatedUser -> String
getAuthenticatedUserName = name

instance FromJSON AuthenticatedUser
instance ToJSON AuthenticatedUser
instance FromJWT AuthenticatedUser 
instance ToJWT AuthenticatedUser

instance ToJSON AuthToken where
  toJSON = toJSON . toString . unAuthToken 

instance UserMonad Handler

-- API
data LoginRequest = LoginRequest
    { username :: !String
    , password :: !String
    } deriving (Show, Eq, Generic)

instance FromJSON LoginRequest
instance ToJSON LoginRequest

newtype AuthToken = AuthToken { unAuthToken :: BS.ByteString } 

type LoginApi = "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] AuthToken
type SessionApi = "renewToken" :> Post '[JSON] AuthToken

type AuthApi auth = (Auth auth AuthenticatedUser :> SessionApi) :<|> LoginApi

authApi 
    :: MonadIO m
    => UserMonad m
    => MonadError ServerError m
    => JWTSettings 
    -> ServerT (AuthApi auth) m
authApi jwt = renewTokenHandler tokenCreator :<|> loginHandler tokenCreator 
  where
    tokenCreator = createJWT jwt

bullshitCrypto :: CryptoSettings
bullshitCrypto = CryptoSettings id

loginHandler
    :: UserMonad m
    => MonadError ServerError m
    => AuthTokenCreator m
    -> LoginRequest
    -> m AuthToken
loginHandler createToken LoginRequest{..} =  do
    user <- login bullshitCrypto username password
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
    userDescription = UserDescription (getAuthenticatedUserId user) (getAuthenticatedUserName user)

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
     -> UserDescription 
     -> m (Maybe AuthToken)
createJWT jwt UserDescription{..} = liftIO $ do
     time <- addUTCTime (3600 :: NominalDiffTime) <$> getCurrentTime
     token <- fmap AuthToken <$> makeJWT (AuthenticatedUser userId userName) jwt (Just time)
     case token of
         Left _ -> return Nothing
         Right t -> return $ Just t
