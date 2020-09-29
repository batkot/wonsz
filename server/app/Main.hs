{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.HashMap.Strict as HM
import Data.IORef
import InMemoryStorage 

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))

import Servant.Auth.Server (generateKey)

import Options (Options, getOptions, optPort, optAllowedCorsOrigin)
import Wonsz.Server (app)
import Wonsz.Storage (KeyValueStorage(..))
import Data.String (fromString)
import Data.List (find)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Reader (runReaderT)

import Wonsz.Users (User(..), UserMonad(..))

main :: IO ()
main = 
    getOptions >>= \case 
        Left err -> putStrLn err
        Right opt -> runServer opt

runServer :: Options -> IO ()
runServer opt = do
    let policy = createCorsPolicy $ optAllowedCorsOrigin opt
    putStrLn $ "Running on port: " <> show (optPort opt)
    jwt <- generateKey
    usrRef <- newIORef users
    run (optPort opt) $ cors (const (Just policy)) $ app (`runReaderT` usrRef) jwt

users :: HM.HashMap String User
users =  HM.insert "Btk" btk HM.empty
  where
    btk = User 1 "Btk" "password" "Tomek" "Batko"

createCorsPolicy :: Maybe String -> CorsResourcePolicy 
createCorsPolicy origin = 
    simpleCorsResourcePolicy { corsRequestHeaders = [ "authorization", "content-type" ], corsOrigins = corsOrigin <$> origin }
  where
    corsOrigin allowedOrigin = ([fromString allowedOrigin], True) 

instance (Monad m, KeyValueStorage m String User) => UserMonad m where 
    getUser = get 
    saveUser user = set (_userName user) user
    -- getById _ = get "Btk"
