{-# LANGUAGE LambdaCase #-}

module Main where

import Network.Wai.Handler.Warp (run)

import Options (Options, getOptions, optPort)
import Server (app)

main :: IO ()
main = 
    getOptions >>= \case 
        Left err -> putStrLn err
        Right opt -> runServer opt

runServer :: Options -> IO ()
runServer opt = do
    putStrLn $ "Running on port: " <> show (optPort opt)
    run (optPort opt) app

