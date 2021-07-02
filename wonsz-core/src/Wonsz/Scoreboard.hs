{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Wonsz.Scoreboard
    ( createNewScoreboard
    , addScoreboardParticipant
    ) where

import Data.Foldable (forM_)
import Control.Monad (foldM)

import Data.Text (Text)
import Data.ByteString (ByteString)
import Lens.Micro.Platform ((^.))

import Wonsz.Scoreboard.Domain
import Wonsz.Identifier
import Wonsz.DomainDrivenDesign

data CreateNewScoreboardCommand = CreateNewScoreboardCommand 
    { scoreboardName :: !Text
    , participantIds :: ![ByteString]
    }

data AddScoreboardParticipantCommand = AddScoreboardParticipantCommand 
    { targetScoreboardId :: !(Id Scoreboard)
    , participantId :: !(Id User)
    }

class Monad m => Repository m key agg where
    find :: key -> m (Maybe agg)
    save :: key -> agg -> m ()

createNewScoreboard 
    :: IdGeneratorMonad m
    => Repository m (Id Scoreboard) Scoreboard
    => CreateNewScoreboardCommand
    -> m ()
createNewScoreboard (CreateNewScoreboardCommand newScoreboardName participants) = do
    result <- createScoreboardAction <$> nextId
    tmpSaveFoo result
  where
    createScoreboardAction id = do
        scoreboard <- newScoreboard newScoreboardName id
        foldM (flip addParticipant) scoreboard (parseId <$> participants)

addScoreboardParticipant
    :: Repository m (Id Scoreboard) Scoreboard
    => AddScoreboardParticipantCommand
    -> m ()
addScoreboardParticipant (AddScoreboardParticipantCommand scoreboardId participantId) = do
    scoreboard <- find scoreboardId
    forM_ (addParticipant participantId <$> scoreboard) tmpSaveFoo

tmpSaveFoo :: Repository m (Id Scoreboard) Scoreboard => ScoreboardAction -> m ()
tmpSaveFoo = dispatch . runAggregateAction 
  where 
    dispatch = either (const (return ())) (\(scoreboard, _) -> save (scoreboard ^. scoreboardId) scoreboard)
