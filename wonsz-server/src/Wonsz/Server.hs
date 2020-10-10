{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Wonsz.Server 
    ( app, Api )
    where

import Wonsz.Server.Authentication (AuthApi, authApi)
import Wonsz.Server.Season (SeasonApi, seasonApi)

import Servant ( Application, ServerT, Proxy(..), (:>), (:<|>)(..), serveWithContext, hoistServerWithContext, Context(..), ServerError, Handler, Raw, serveDirectoryWebApp)
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultJWTSettings, defaultCookieSettings, JWT)

import Crypto.JOSE.JWK (JWK)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error.Class (MonadError)

import Wonsz.Users (UserMonad)
import Wonsz.Crypto (CryptoMonad)

type Api auth = 
    "api" :> SeasonApi auth 
    :<|> "auth" :> AuthApi auth
    :<|> "static" :> Raw

server 
    :: MonadIO m 
    => MonadError ServerError m
    => UserMonad m
    => CryptoMonad m
    => JWTSettings 
    -> ServerT (Api auth) m
server jwt = seasonApi :<|> authApi jwt :<|> serveDirectoryWebApp "static"

app :: MonadIO m 
    => MonadError ServerError m
    => UserMonad m
    => CryptoMonad m
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
