{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Wonsz.Server 
    ( app, Api )
    where

import Wonsz.Server.Authentication (AuthApi, authApi)
import Wonsz.Server.Season (SeasonApi, seasonApi)

import Servant ( Application, ServerT, Proxy(..), (:>), (:<|>)(..), serveWithContext, hoistServerWithContext, Context(..), ServerError, Handler)
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultJWTSettings, defaultCookieSettings, JWT)

import Crypto.JOSE.JWK (JWK)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error.Class (MonadError)

import Wonsz.Users (UserMonad)

type Api auth = "api" :> SeasonApi auth :<|> "auth" :> AuthApi auth

server 
    :: MonadIO m 
    => MonadError ServerError m
    => UserMonad m
    => JWTSettings 
    -> ServerT (Api auth) m
server jwt = seasonApi :<|> authApi jwt

makeProxy :: a -> Proxy a
makeProxy = const Proxy

app :: MonadIO m 
    => MonadError ServerError m
    => UserMonad m
    => (forall x. m x -> Handler x)
    -> JWK 
    -> Application
app runMonadStack key = 
    serveWithContext api context $ 
        hoistServerWithContext api settingsProxy runMonadStack $ server jwtSettings
  where 
    api :: Proxy (Api '[JWT])
    api = Proxy
    jwtSettings = defaultJWTSettings key
    context = defaultCookieSettings :. jwtSettings :. EmptyContext
    settingsProxy = Proxy :: Proxy '[CookieSettings, JWTSettings]
