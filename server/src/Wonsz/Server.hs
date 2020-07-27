{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Wonsz.Server 
    ( app )
    where

import Wonsz.Server.Authentication (AuthenticatedUser, getAuthenticatedUserId, getAuthenticatedUserName, AuthApi, authApi)

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

-- Wonsz Api
type WonszApi = "overview" :> Get '[JSON] String

wonszApi :: AuthResult AuthenticatedUser -> Server WonszApi
wonszApi (Authenticated user) = return $ "Hello " <> getAuthenticatedUserName user
wonszApi _ = throwAll err401

-- Api
type Api auth = (Auth auth AuthenticatedUser :> WonszApi) :<|> AuthApi auth

server :: JWTSettings -> Server (Api auth)
server jwt = wonszApi :<|> authApi jwt

app :: JWK -> Application
app key = serveWithContext api context $ server jwtSettings
  where 
    api :: Proxy (Api '[JWT])
    api = Proxy
    jwtSettings = defaultJWTSettings key
    context = defaultCookieSettings :. jwtSettings :. EmptyContext
