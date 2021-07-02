{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

module Wonsz.Identifier
    ( Id
    , parseId
    , UnusedId
    , pattern UnusedId
    , IdGeneratorMonad(..)
    , ConvertableIds(..)
    ) where

import Control.Monad.Trans.Class (MonadTrans(..))

import Data.ByteString (ByteString)

newtype Id a = Id { unId :: ByteString } deriving stock (Eq, Show)

newtype UnusedId a = MkUnusedId { getId :: Id a } deriving stock (Eq, Show)

pattern UnusedId :: Id a -> UnusedId a
pattern UnusedId id <- MkUnusedId id

parseId :: ByteString -> Id a
parseId = Id

class ConvertableIds a b where
    convertId :: Id a -> Id b
    convertId = Id . unId

class Monad m => IdGeneratorMonad m where
    nextId :: m (UnusedId a)

instance {-# OVERLAPPABLE #-}
    ( IdGeneratorMonad m
    , MonadTrans t
    , Monad (t m)) => IdGeneratorMonad (t m) where
    nextId = lift nextId
