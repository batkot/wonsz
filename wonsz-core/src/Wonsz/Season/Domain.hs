{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}

module Wonsz.Season.Domain
    ( Season
    , newSeason
    , scorePoint

    ) where

import Lens.Micro.Platform (makeLenses, (^.))

import Data.Text
import Wonsz.Identifier 

-- Season
-- scorePoint :: Yada yada -> Season -> Point
-- voteForPoint :: Yada yada -> Point -> Point
--
-- findSeason seasonId >>= scorePoint score >>= savePoint
-- findPoint pointId >>= voteForPoint vote >>= savePoint
--

data Participant

data Season = Season
    { _seasonName :: !Text
    , _seasonActive :: !Bool
    , _seasonId :: !(Id Season)
    } 
    deriving stock (Eq, Show)

makeLenses ''Season

newSeason 
    :: IdGeneratorMonad m
    => Text 
    -> m Season
newSeason seasonName = Season seasonName True <$> nextId

data PointVote = PointVote
    { _pointVoteValue :: !Int
    , _pointVoteVoterId :: !(Id Participant)
    } deriving stock (Eq, Show)

makeLenses ''PointVote

data Point = Point
    { _pointTitle :: !Text
    , _pointSeasonId :: !(Id Season)
    , _pointVotes :: ![PointVote]
    , _pointId :: !(Id Point)
    }
    deriving stock (Eq, Show)

makeLenses ''Point

scorePoint 
    :: IdGeneratorMonad m
    => Text 
    -> Season 
    -> m Point
scorePoint title season = 
    Point title (season ^. seasonId) [] <$> nextId
