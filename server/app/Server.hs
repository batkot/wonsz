{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Server 
    ( app )
    where

import Servant ( Application, Server, JSON, ReqBody, Get, Post, Proxy(..), (:>), (:<|>)(..), serveWithContext, Handler, err401, err403, Context(..), ServerT, ServerError)
import Servant.Auth.Server (FromJWT, ToJWT, Auth, AuthResult(..), makeJWT, JWTSettings, throwAll, defaultJWTSettings, defaultCookieSettings, JWT)

import Crypto.JOSE.JWK (JWK)

import Data.Time.Clock (getCurrentTime, addUTCTime, NominalDiffTime)

import Data.ByteString.Lazy.UTF8 as BSL

import GHC.Generics
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Error.Class (MonadError(..))

import Data.Aeson (FromJSON, ToJSON)
import Data.Either

import Wonsz.Users (login, UserMonad(..), CryptoSettings(..), UserDescription(..))

data AuthorizedUser = AuthorizedUser
    { auId :: !Int
    , name :: !String
    } deriving (Show, Eq, Generic, Read)

instance FromJSON AuthorizedUser
instance ToJSON AuthorizedUser
instance FromJWT AuthorizedUser 
instance ToJWT AuthorizedUser

data LoginData = LoginData
    { user :: !String
    , password :: !String
    } deriving (Show, Eq, Generic)

instance FromJSON LoginData
instance ToJSON LoginData

-- Authentication
type AuthenticationApi = "login" :> ReqBody '[JSON] LoginData :> Post '[JSON] String

type CommandHandler m cmd res = cmd -> m res

type UserAuth m = CommandHandler m LoginData (Either String AuthorizedUser)

instance UserMonad Handler 

userAuth :: Monad m => UserAuth m
userAuth = return . Right . AuthorizedUser 1 . user

bullshitCrypto :: CryptoSettings
bullshitCrypto = CryptoSettings id

type AuthTokenCreator m = UserDescription -> m (Maybe String)

loginHandler
    :: UserMonad m
    => MonadError ServerError m
    => AuthTokenCreator m
    -> CommandHandler m LoginData String
loginHandler createToken LoginData{..} =  do
    user <- login bullshitCrypto user password
    token <- case user of
        Nothing -> throwError err401
        Just u -> createToken u
    case token of
        Nothing -> throwError err401
        Just t -> return t

createJWT :: MonadIO m => JWTSettings -> UserDescription -> m (Maybe String)
createJWT jwt UserDescription{..} = liftIO $ do
    time <- addUTCTime (3600 :: NominalDiffTime) <$> getCurrentTime
    token <- fmap BSL.toString <$> makeJWT (AuthorizedUser userId userName) jwt (Just time)
    case token of
        Left _ -> return Nothing
        Right t -> return $ Just t

authenticationApi
    :: MonadIO m 
    => UserMonad m
    => MonadError ServerError m
    => JWTSettings 
    -> ServerT AuthenticationApi m
authenticationApi jwt = loginHandler (createJWT jwt)

-- Wonsz Api
type WonszApi = "overview" :> Get '[JSON] String

wonszApi :: AuthResult AuthorizedUser -> Server WonszApi
wonszApi (Authenticated user) = return $ "Hello " <> name user
wonszApi _ = throwAll err401

-- Api
type Api auth = (Auth auth AuthorizedUser :> WonszApi) :<|> AuthenticationApi

server :: JWTSettings -> Server (Api auth)
server jwt = wonszApi :<|> authenticationApi jwt 

app :: JWK -> Application
app key = serveWithContext api context $ server jwtSettings
  where 
    api :: Proxy (Api '[JWT])
    api = Proxy
    jwtSettings = defaultJWTSettings key
    context = defaultCookieSettings :. jwtSettings :. EmptyContext
