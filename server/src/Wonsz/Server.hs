{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Wonsz.Server 
    ( app, Api )
    where

import Wonsz.Server.Authentication (AuthApi, authApi)
import Wonsz.Server.Season (SeasonApi, seasonApi)

import Servant ( Application, Server, Proxy(..), (:>), (:<|>)(..), serveWithContext, Context(..))
import Servant.Auth.Server (JWTSettings, defaultJWTSettings, defaultCookieSettings, JWT)

import Crypto.JOSE.JWK (JWK)

type Api auth = "api" :> SeasonApi auth :<|> "auth" :> AuthApi auth

server :: JWTSettings -> Server (Api auth)
server jwt = seasonApi :<|> authApi jwt

app :: JWK -> Application
app key = serveWithContext api context $ server jwtSettings
  where 
    api :: Proxy (Api '[JWT])
    api = Proxy
    jwtSettings = defaultJWTSettings key
    context = defaultCookieSettings :. jwtSettings :. EmptyContext
