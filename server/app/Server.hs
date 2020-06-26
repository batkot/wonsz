{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Server 
    ( app )
    where

import Servant ( Application, Server, JSON, Get, Proxy(..), (:>), (:<|>)(..), serve)

type Api = "hey" :> Get '[JSON] String

api :: Proxy Api
api = Proxy

server :: Server Api
server = return "Hello"

app :: Application
app = serve api server


