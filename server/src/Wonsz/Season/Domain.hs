{-# LANGUAGE DerivingStrategies #-}

module Wonsz.Season.Domain
    ( Season
    , newSeason
    , scorePoint

    ) where

import Data.Text
import Wonsz.Identifier 

import Prelude hiding (id)

-- Season
-- scorePoint :: Yada yada -> Season -> Point
-- voteForPoint :: Yada yada -> Point -> Point
--
-- findSeason seasonId >>= scorePoint score >>= savePoint
-- findPoint pointId >>= voteForPoint vote >>= savePoint

data Season = Season
    { name :: !Text
    , active :: !Bool
    , id :: !(Id Season)
    } 
    deriving stock (Eq, Show)

newSeason 
    :: IdGeneratorMonad m
    => Text 
    -> m Season
newSeason seasonName = Season seasonName True <$> nextId

data Point = Point
    { title :: !Text
    , seasonId :: !(Id Season)
    , pId :: !(Id Point)
    }
    deriving stock (Eq, Show)

scorePoint 
    :: IdGeneratorMonad m
    => Text 
    -> Season 
    -> m Point
scorePoint title season = Point title (id season) <$> nextId
