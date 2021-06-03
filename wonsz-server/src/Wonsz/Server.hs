{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Wonsz.Server
    ( app, Api )
    where

import Wonsz.Server.Authentication (Protected, AuthApi, authApi, AuthenticatedUser, protected2)
import Wonsz.Server.Season (SeasonApi, seasonApi)
import Wonsz.Server.Account (AccountApi, accountApi, accountApi)

import Servant ( Application, ServerT, Proxy(..), (:>), (:<|>)(..), serveWithContext, hoistServerWithContext, Context(..), ServerError, Handler, Raw, serveDirectoryWebApp, err401)
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultJWTSettings, defaultCookieSettings, JWT, AuthResult(..), ThrowAll(..))

import Crypto.JOSE.JWK (JWK)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error.Class (MonadError(..))

import Wonsz.Users (UserMonad)
import Wonsz.Crypto (CryptoMonad)

type AppApi auth =
    "season" :> SeasonApi auth 
    :<|> AccountApi auth

type Api auth =
    ("api" :> AppApi auth)
    :<|> "auth" :> AuthApi auth
    :<|> "static" :> Raw

server
    :: MonadIO m
    => MonadError ServerError m
    => UserMonad m
    => CryptoMonad m
    => JWTSettings
    -> ServerT (Api auth) m
server jwt = api :<|> authApi jwt :<|> serveDirectoryWebApp "static"
  where
    api = seasonApi :<|> accountApi

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
    settingsProxy :: Proxy '[CookieSettings, JWTSettings]
    settingsProxy = Proxy
