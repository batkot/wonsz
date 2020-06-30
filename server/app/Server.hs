{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Server 
    ( app )
    where

import Servant ( Application, Server, JSON, ReqBody, Get, Post, Proxy(..), (:>), (:<|>)(..), serveWithContext, Handler, err401, Context(..))
import Servant.Auth.Server (FromJWT, ToJWT, Auth, AuthResult(..), makeJWT, JWTSettings, throwAll, defaultJWTSettings, defaultCookieSettings, JWT)

import Crypto.JOSE.JWK (JWK)

import Data.ByteString.Lazy.UTF8 as BSL

import GHC.Generics
import Control.Monad.IO.Class (liftIO)

import Data.Aeson (FromJSON, ToJSON)

data AuthorizedUser = AuthorizedUser
    { id :: !Int
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

type AuthenticationApi = "login" :> ReqBody '[JSON] LoginData :> Post '[JSON] String

type WonszApi = "overview" :> Get '[JSON] String

type Api auth = (Auth auth AuthorizedUser :> WonszApi) :<|> AuthenticationApi

wonszApi :: AuthResult AuthorizedUser -> Server WonszApi
wonszApi (Authenticated user) = return $ "Hello " <> name user
wonszApi _ = throwAll err401

loginHandler :: JWTSettings -> LoginData -> Handler String
loginHandler jwt loginData = do
    token <- liftIO $ makeJWT (AuthorizedUser 1 (user loginData)) jwt Nothing
    return $ either (const "") BSL.toString token

authenticationApi :: JWTSettings -> Server AuthenticationApi
authenticationApi = loginHandler 

server :: JWTSettings -> Server (Api auth)
server jwt = wonszApi :<|> authenticationApi jwt

app :: JWK -> Application
app key = serveWithContext api context $ server jwtSettings
  where 
    api :: Proxy (Api '[JWT])
    api = Proxy
    jwtSettings = defaultJWTSettings key
    context = defaultCookieSettings :. jwtSettings :. EmptyContext
