{-# LANGUAGE RankNTypes #-}

module Wonsz.Named 
    ( Named 
    , unNamed

    , named
    ) where

newtype Named name a = Named { unNamed :: a }

named :: a -> (forall name. Named name a -> r) -> r
named x f = f . Named $ x
