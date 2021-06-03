{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Wonsz.Server.Dashboard 
    ( DashboardApi
    , dashboardApi 
    ) where

import Data.Text (Text, pack)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=))
import GHC.Generics (Generic)

import Control.Monad.Error.Class (MonadError, throwError)

import Servant ((:>), Get, JSON, ServerT, ServerError, Capture, err401)
import Servant.Auth.Server (Auth, AuthResult(..))

import Wonsz.Server.Authentication (Protected, protected, AuthenticatedUser, getAuthenticatedUserId, protected2)
import Wonsz.Server.Account (AccountDetails(..))

data ScoreboardSummary = ScoreboardSummary
    { ssId :: !Int
    , ssName :: !Text
    , ssParticipantsCount :: !Int
    , ssLeader :: AccountDetails
    }
    deriving stock (Show, Eq, Generic)

instance ToJSON ScoreboardSummary where
      toJSON (ScoreboardSummary id name count leader) = object
        [ "id" .= id
        , "name" .= name
        , "participantsCount" .= count
        , "leader" .= toJSON leader
        ]

type UserDashboard = [ScoreboardSummary]

type DashboardApi auth = "dashboard" :> Protected auth :> DashboardApi'

type DashboardApi' = 
    Capture "id" Int :> Get '[JSON] UserDashboard

dashboardApi 
    :: Monad m
    => MonadError ServerError m
    => AuthResult AuthenticatedUser
    -> ServerT DashboardApi' m
dashboardApi (Authenticated user) = dashboardHandler 
dashboardApi _ = const $ throwError err401

dashboardHandler :: Monad m => Int -> m UserDashboard
dashboardHandler _ = return $ fakeScoreboard <$> [1..10]
  where
    fakeScoreboard id = ScoreboardSummary id (pack ("Scoreboard " <> show id)) 10 $ AccountDetails 1 "btk" "Tomasz Batko" "/static/btk.jpg"
