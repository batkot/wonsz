{-# LANGUAGE MultiParamTypeClasses #-}

module Wonsz.Storage 
    ( KeyValueStorage(..)
    ) where

class Monad m => KeyValueStorage m key value where
    get :: key -> m (Maybe value)
    set :: key -> value -> m ()
