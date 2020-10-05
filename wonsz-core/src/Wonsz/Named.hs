{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Wonsz.Named 
    ( Named 
    , unNamed
    , pattern Named

    , name
    ) where

newtype Named name a = MkNamed { unNamed :: a }

pattern Named :: a -> Named name a
pattern Named a <- MkNamed a

name :: a -> (forall name. Named name a -> r) -> r
name x f = f . MkNamed $ x
