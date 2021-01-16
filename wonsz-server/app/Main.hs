{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad.Logger (runStderrLoggingT)

import Wonsz.Storage.Postgres.Database (initializePostgresqlPool, runPostgresBackEndT)

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))

import Servant.Auth.Server (generateKey)

import Options (Options, getOptions, optPort, optAllowedCorsOrigin)
import Wonsz.Server (app)
import Data.ByteString (ByteString)
import Data.String (fromString)
import Control.Monad.Error.Class (MonadError)

import Wonsz.Crypto (runPlainTextCryptoT, PlainTextCryptoT(..))

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
    pool <- runStderrLoggingT $ initializePostgresqlPool connString 2
    run (optPort opt) $ cors (const (Just policy)) $ app (runPlainTextCryptoT . runPostgresBackEndT pool) jwt

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

deriving newtype instance MonadError err m => MonadError err (PlainTextCryptoT m)
