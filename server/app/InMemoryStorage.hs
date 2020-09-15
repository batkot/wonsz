{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module InMemoryStorage 
    (
    ) where

import qualified Data.HashMap.Strict as HM
import Data.IORef

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))

import Wonsz.Storage (KeyValueStorage(..))

instance (MonadIO m, MonadReader (IORef (HM.HashMap String value)) m) => KeyValueStorage m String value where
    get key = do 
        ioRef <- ask
        map <- liftIO (readIORef ioRef)
        return $ HM.lookup key map

    set key value = do
        ioRef <- ask
        liftIO $ modifyIORef ioRef $ HM.insert key value
