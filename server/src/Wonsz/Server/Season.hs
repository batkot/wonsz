{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Wonsz.Server.Season 
    ( SeasonApi
    , seasonApi

    , SeasonOverview(..)
    , ParticipantOverview (..)
    ) where

import Servant ((:>), Get, JSON, ServerT, ServerError)
import Servant.Auth.Server (Auth)
import Wonsz.Server.Authentication (protected, AuthenticatedUser)

import GHC.Generics
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)

data SeasonOverview = SeasonOverview 
    { participants :: [ParticipantOverview]
    } deriving (Show, Eq, Generic)

instance ToJSON SeasonOverview 
instance FromJSON SeasonOverview 

data ParticipantOverview = ParticipantOverview
    { participantName :: !Text
    , participantScore :: !Integer
    , participantPlace :: !Integer
    , participantAvatarUrl :: !Text
    } deriving (Show, Eq, Generic)

instance ToJSON ParticipantOverview 
instance FromJSON ParticipantOverview

type SeasonApi auth = Auth auth AuthenticatedUser :> "overview" :> Get '[JSON] SeasonOverview

seasonApi 
    :: MonadIO m 
    => MonadError ServerError m
    =>  ServerT (SeasonApi auth) m
seasonApi = protected $ const overviewHandler 

overviewHandler :: Monad m => m SeasonOverview
overviewHandler = return $ SeasonOverview participants
    where
      participants = 
          [ ParticipantOverview "Paweł Machay" 12 1 "/static/makkay.jpg"
          , ParticipantOverview "Hubert Kotlarz" 10 2 "/static/hubert.jpg"
          , ParticipantOverview "Tomasz Batko" 8 3 "/static/btk.jpg"
          , ParticipantOverview "Jakub Dziedzic" 7 4 "/static/kuba.jpg"
          , ParticipantOverview "Paweł Szuro" 6 5 "/static/szuro.jpg"
          , ParticipantOverview "Mateusz Wałach" 3 6 "/static/mateusz.jpg"
          ] 
