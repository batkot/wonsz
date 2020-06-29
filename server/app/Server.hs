{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Server 
    ( app )
    where

import Servant ( Application, Server, JSON, ReqBody, Get, Post, Proxy(..), (:>), (:<|>)(..), serve, Handler)

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

data LoginData = LoginData
    { user :: String
    , password :: String
    } deriving (Show, Eq, Generic)

instance FromJSON LoginData
instance ToJSON LoginData

type Api = "login" :> ReqBody '[JSON] LoginData :> Post '[JSON] String

api :: Proxy Api
api = Proxy

server :: Server Api
server = login
    where
      login :: LoginData -> Handler String
      login d = return (user d)

app :: Application
app = serve api server


