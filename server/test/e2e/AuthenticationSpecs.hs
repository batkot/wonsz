{-# LANGUAGE DataKinds #-}

module AuthenticationSpecs
    ( test_tests
    ) where

import Control.Concurrent (forkIO, killThread, ThreadId)
import Data.Either

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC

import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (testCase, (@?=), (@?))
import Test.Tasty.QuickCheck (testProperty, ioProperty)
import Test.QuickCheck (Arbitrary(..), getPrintableString)
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

import Wonsz.Users (UserMonad(..), User(..))

import Control.Monad.Identity (IdentityT, runIdentityT)

makeWonszAppResource :: IO (Port, ThreadId)
makeWonszAppResource = do
    (port, socket) <- Warp.openFreePort
    jwt <- generateKey
    threadId <- forkIO $ Warp.runSettingsSocket defaultSettings socket (app runIdentityT jwt)
    return (port, threadId)

instance Monad m => UserMonad (IdentityT m) where
  getUser userName = return . Just $ User 1 userName "password"

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

                , testProperty "Given fake token should return 401" $ \(BadAuthToken badTokenString) -> ioProperty $ do 
                    c <- client
                    let badToken = Token . BSL.toStrict . BSLC.pack $ badTokenString
                    Left (FailureResponse _ (Response responseStatus _ _ _)) <- runClientM (getOverview c badToken) (env c)
                    return $ responseStatus == status401
                ]

newtype BadAuthToken = BadAuthToken { getToken :: String } deriving (Show)

instance Arbitrary BadAuthToken where
  arbitrary = BadAuthToken . mconcat . lines . getPrintableString <$> arbitrary
