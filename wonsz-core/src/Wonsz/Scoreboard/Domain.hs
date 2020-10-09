{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}

module Wonsz.Scoreboard.Domain
    ( Scoreboard
    , newScoreboard
    , addParticipant

    ) where

import Lens.Micro.Platform (makeLenses, (^.), (<>~), (&), (.~))

import Data.Text (Text)
import Wonsz.Identifier 

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
    deriving stock (Eq, Show)

data Scoreboard = Scoreboard
    { _scoreboardName :: !Text
    , _scoreboardActive :: !Bool
    , _scoreboardParticipants :: ![Participant]
    , _scoreboardId :: !(Id Scoreboard)
    } 
    deriving stock (Eq, Show)

makeLenses ''Scoreboard

newScoreboard
    :: IdGeneratorMonad m
    => Text 
    -> m Scoreboard
newScoreboard scoreboardName = Scoreboard scoreboardName True [] <$> nextId

addParticipant :: Id User -> Scoreboard -> Scoreboard
addParticipant userId board
  | alreadyParticipates = board
  | otherwise = board & scoreboardParticipants <>~ [newParticipant]
    where
      participantId = convertId userId
      alreadyParticipates = board ^. scoreboardParticipants & any ((==) participantId . _participantId)
      newParticipant = Participant participantId 0

