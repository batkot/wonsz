{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Wonsz.Scoreboard
    ( createNewScoreboard
    , addScoreboardParticipant
    ) where

import Data.Foldable (forM_)

import Data.Text (Text)
import Lens.Micro.Platform ((^.))

import Wonsz.Scoreboard.Domain
import Wonsz.Identifier

newtype CreateNewScoreboardCommand = CreateNewScoreboardCommand { scoreboardName :: Text }
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
createNewScoreboard (CreateNewScoreboardCommand newScoreboardName) = do
    scoreboard <- newScoreboard newScoreboardName
    save (scoreboard ^. scoreboardId) scoreboard

addScoreboardParticipant
    :: Repository m (Id Scoreboard) Scoreboard
    => AddScoreboardParticipantCommand
    -> m ()
addScoreboardParticipant (AddScoreboardParticipantCommand scoreboardId participantId) = do
    scoreboard <- find scoreboardId
    forM_ (addParticipant participantId <$> scoreboard) (save scoreboardId)
