{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.HashMap.Strict as HM
import Data.IORef

import Control.Monad.Logger (runStderrLoggingT, LoggingT)

import Wonsz.Storage.InMemory.KeyValueStorage
import Wonsz.Storage.InMemory.Repositories

import Wonsz.Storage.Postgres.Database (initialize, runPostgresBackEndT)

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))

import Database.Persist.Postgresql 

import Servant.Auth.Server (generateKey)
import Servant (ServerError)

import Options (Options, getOptions, optPort, optAllowedCorsOrigin)
import Wonsz.Server (app)
import Data.ByteString (ByteString)
import Data.String (fromString)
import Data.List (find)
import Data.Text (unpack, pack)

import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Identity (IdentityT(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Reader (runReaderT)

import Wonsz.Users.Domain (User(..)) -- hot wiring, should be fixed

main :: IO ()
main = 
    getOptions >>= \case 
        Left err -> putStrLn err
        Right opt -> runServer opt

connString :: ByteString
connString = "postgres://wonsz-web:dupa@localhost:5432/persistent-test"

runServer :: Options -> IO ()
runServer opt = do
    let policy = createCorsPolicy $ optAllowedCorsOrigin opt
    putStrLn $ "Running on port: " <> show (optPort opt)
    jwt <- generateKey
    pool <- runStderrLoggingT $ createPostgresqlPool connString 10
    runSqlPool initialize pool
    run (optPort opt) $ cors (const (Just policy)) $ app (runPostgresBackEndT pool) jwt

-- inMemoryStack =
--     runInMemoryKvsT users . runKvsUserMonadT 
--   where
--     users =  HM.fromList usersList
--       where
--         usersList =
--             [ User 1 "Btk" "Tomek" "password"
--             , User 2 "Makkay" "Makkay" "pswd"
--             ] >>= \u -> [(show (_userId u), u), (show (_userLogin u), u)]

createCorsPolicy :: Maybe String -> CorsResourcePolicy 
createCorsPolicy origin = 
    simpleCorsResourcePolicy { corsRequestHeaders = [ "authorization", "content-type" ], corsOrigins = corsOrigin <$> origin }
  where
    corsOrigin allowedOrigin = ([fromString allowedOrigin], True) 
