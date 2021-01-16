{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}

module Wonsz.Identifier
    ( Id
    , IdGeneratorMonad(..)
    , ConvertableIds(..)
    ) where

import Control.Monad.Trans.Class (MonadTrans(..))

import Data.ByteString (ByteString)

newtype Id a = Id { unId :: ByteString } deriving stock (Eq, Show)

class ConvertableIds a b where
    convertId :: Id a -> Id b
    convertId = Id . unId

class Monad m => IdGeneratorMonad m where
    nextId :: m a

instance {-# OVERLAPPABLE #-}
    ( IdGeneratorMonad m
    , MonadTrans t
    , Monad (t m)) => IdGeneratorMonad (t m) where
    nextId = lift nextId

