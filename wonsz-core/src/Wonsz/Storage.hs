{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Wonsz.Storage 
    ( KeyValueStorage(..)
    ) where

import Control.Monad.Trans.Class (MonadTrans(..))

class Monad m => KeyValueStorage m key value where
    get :: key -> m (Maybe value)
    set :: key -> value -> m ()

instance {-# OVERLAPPABLE #-}
    ( KeyValueStorage m k v
    , MonadTrans t
    , Monad (t m)) => KeyValueStorage (t m) k v where
    get = lift . get
    set k v = lift $ set k v
