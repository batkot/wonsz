{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

module Wonsz.Named 
    ( Named 
    , unNamed
    , pattern Named

    , named
    ) where

newtype Named name a = MkNamed { unNamed :: a }

pattern Named :: a -> Named name a
pattern Named a <- MkNamed a

named :: a -> (forall name. Named name a -> r) -> r
named x f = f . MkNamed $ x
