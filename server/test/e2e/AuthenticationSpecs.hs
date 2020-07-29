{-# LANGUAGE DataKinds #-}

module AuthenticationSpecs
    ( test_tests
    ) where

import Control.Concurrent (forkIO, killThread, ThreadId)
import Data.Either

import qualified Data.ByteString.Lazy as BSL

import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (testCase, (@?=), (@?))
import Network.Wai.Handler.Warp as Warp

import Servant (Proxy(..), (:<|>)(..))
import Servant.Auth.Server (generateKey, JWT)
import Servant.Client 
import Servant.Auth.Client
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types.Status (status401)

import Wonsz.Server (app, Api)
import Wonsz.Server.Authentication (AuthToken, LoginRequest(..), rawToken)
import Wonsz.Server.Season (SeasonOverview(..))

makeWonszAppResource :: IO (Port, ThreadId)
makeWonszAppResource = do
    (port, socket) <- Warp.openFreePort
    putStrLn $ "Running on " <> show port 
    jwt <- generateKey
    threadId <- forkIO $ Warp.runSettingsSocket defaultSettings socket (app jwt)
    return (port, threadId)

freeWonszAppResource :: (Port, ThreadId) -> IO ()
freeWonszAppResource = killThread . snd

-- type Api auth = "api" :> SeasonApi auth :<|> "auth" :> AuthApi auth
--
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
    let overviewClient :<|> (renewTokenClient :<|> loginClient) = client testApi
    baseUrl <- parseBaseUrl "http://localhost"
    manager <- newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager (baseUrl { baseUrlPort = port })
    return $ WonszClient loginClient renewTokenClient overviewClient clientEnv

test_tests :: TestTree
test_tests = 
    withResource makeWonszAppResource freeWonszAppResource $ \x ->
        withResource (fst <$> x >>= createApiClient) (const  (return ())) $ \client -> 
            testGroup "Login workflow tests" 
                [ testCase "Given wrong credentails should raise 401" $ do
                    c <- client
                    Left (FailureResponse _ (Response responseStatus _ _ _)) <- runClientM ((login c) (LoginRequest "dupa" "dupa")) (env c)
                    responseStatus @?= status401

                , testCase "Given good credentials should return working token" $ do
                    c <- client
                    Right authToken <- runClientM ((login c) (LoginRequest "dupa" "password")) (env c)
                    let token = Token . BSL.toStrict . rawToken $ authToken
                    result <- runClientM (getOverview c token) (env c)
                    isRight result @? "Should get result"

                , testCase "Token renewal return new working token" $ do 
                    c <- client
                    Right authToken <- runClientM ((login c) (LoginRequest "dupa" "password")) (env c)
                    let token = Token . BSL.toStrict . rawToken $ authToken
                    Right renewedToken <- runClientM (renewToken c token) (env c)
                    let newToken = Token . BSL.toStrict . rawToken $ renewedToken
                    result <- runClientM (getOverview c newToken) (env c)
                    isRight result @? "Should get result"
                ]
