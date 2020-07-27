{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))

import Servant.Auth.Server (generateKey)

import Options (Options, getOptions, optPort, optAllowedCorsOrigin)
import Wonsz.Server (app)
import Data.String (fromString)

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
    run (optPort opt) $ cors (const (Just policy)) $ app jwt
    

createCorsPolicy :: Maybe String -> CorsResourcePolicy 
createCorsPolicy origin = 
    simpleCorsResourcePolicy { corsRequestHeaders = [ "authorization", "content-type" ], corsOrigins = corsOrigin <$> origin }
  where
    corsOrigin allowedOrigin = ([fromString allowedOrigin], True) 

