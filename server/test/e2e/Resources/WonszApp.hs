{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Resources.WonszApp 
    ( withWonszApp

    , makeWonszAppResource
    , freeWonszAppResource
    , createApiClient
    , WonszClient(..)

    , validLoginRequest 
    , invalidLoginRequest 
    ) where

import Data.ByteString.Char8 (pack)

import Test.Tasty (TestTree, testGroup, withResource)

import Control.Monad.Identity (IdentityT, runIdentityT)
import Control.Concurrent (forkIO, killThread, ThreadId)

import Network.Wai.Handler.Warp as Warp
import Network.HTTP.Client (newManager, defaultManagerSettings)

import Servant (Proxy(..), (:<|>)(..))
import Servant.Client 
import Servant.Auth.Server (generateKey, JWT)
import Servant.Auth.Client

import Wonsz.Server (app, Api)
import Wonsz.Server.Authentication (AuthToken, LoginRequest(..), rawToken)
import Wonsz.Server.Season (SeasonOverview(..))

import Wonsz.Users (UserMonad(..))
import Wonsz.Users.Domain (User(..))

withWonszApp :: (IO WonszClient -> TestTree) -> TestTree
withWonszApp tests = 
    withResource makeWonszAppResource freeWonszAppResource $ \x ->
        withResource (fst <$> x >>= createApiClient) (const  (return ())) tests

makeWonszAppResource :: IO (Port, ThreadId)
makeWonszAppResource = do
    (port, socket) <- Warp.openFreePort
    jwt <- generateKey
    threadId <- forkIO $ Warp.runSettingsSocket defaultSettings socket (app runIdentityT jwt)
    return (port, threadId)

freeWonszAppResource :: (Port, ThreadId) -> IO ()
freeWonszAppResource = killThread . snd

testApi :: Proxy (Api '[JWT])
testApi = Proxy

data WonszClient = WonszClient
    { login :: LoginRequest -> ClientM AuthToken
    , renewToken :: Token -> ClientM AuthToken
    , getOverview :: Token -> ClientM SeasonOverview
    , env :: ClientEnv
    }

createApiClient :: Port -> IO WonszClient
createApiClient port = do
    let overviewClient :<|> ((renewTokenClient :<|> changePassword) :<|> loginClient) :<|> _ = client testApi
    baseUrl <- parseBaseUrl "http://localhost"
    manager <- newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager (baseUrl { baseUrlPort = port })
    return $ WonszClient loginClient renewTokenClient overviewClient clientEnv

----
userName :: String
userName = "whatever"

goodPassword :: String
goodPassword = "password"

badPassword :: String
badPassword = "notPassword"

validLoginRequest :: LoginRequest
validLoginRequest = LoginRequest userName goodPassword

invalidLoginRequest :: LoginRequest 
invalidLoginRequest = LoginRequest userName badPassword

instance Monad m => UserMonad (IdentityT m) where
  getUser userName = return . Just $ User 1 userName "" (pack goodPassword) 
