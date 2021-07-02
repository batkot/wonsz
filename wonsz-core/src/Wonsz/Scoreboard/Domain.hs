{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}

module Wonsz.Scoreboard.Domain
    ( Scoreboard
    , ScoreboardAction
    , scoreboardId

    , newScoreboard
    , addParticipant

    , User
    ) where

import Lens.Micro.Platform

import Data.Text (Text)
import Wonsz.Identifier
import Wonsz.DomainDrivenDesign
import GHC.Generics (Generic)

-- Scoreboard
-- scorePoint :: Yada yada -> Scoreboard -> Point
-- voteForPoint :: Yada yada -> Point -> Point
--
-- findScoreboard scoreboardId >>= scorePoint score >>= savePoint
-- findPoint pointId >>= voteForPoint vote >>= savePoint

data User = User

instance ConvertableIds User Participant

data Participant = Participant
    { _participantId :: !(Id Participant)
    , _participantScore :: !Int
    }
    deriving stock (Eq, Show, Generic)

data Scoreboard = Scoreboard
    { _scoreboardName :: !Text
    , _scoreboardActive :: !Bool
    , _scoreboardParticipants :: ![Participant]
    , _scoreboardId :: !(Id Scoreboard)
    }
    deriving stock (Eq, Show, Generic)

makeLenses ''Scoreboard

instance Aggregate Scoreboard () ()

type ScoreboardAction = AggregateAction () () Scoreboard

newScoreboard
    :: Text
    -> UnusedId Scoreboard
    -> ScoreboardAction
newScoreboard scoreboardName (UnusedId id) = do 
    raiseEvent ()
    return $ Scoreboard scoreboardName True [] id

addParticipant :: Id User -> Scoreboard -> ScoreboardAction
addParticipant userId board
  | alreadyParticipates = return board
  | otherwise = return $ board & scoreboardParticipants <>~ [newParticipant]
    where
      participantId = convertId userId
      alreadyParticipates = board ^. scoreboardParticipants & any ((==) participantId . _participantId)
      newParticipant = Participant participantId 0
