module App where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Dict (Dict)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as EC
import Logger (class MonadLogger)

newtype AppT :: forall k. (k -> Type) -> k -> Type
newtype AppT m a = AppT (m a)

runAppT :: forall m a. AppT m a -> m a
runAppT (AppT app) = app

derive newtype instance appTFunctor :: Functor f => Functor (AppT f)
derive newtype instance appTApply :: Apply m => Apply (AppT m)
derive newtype instance appTApplicative :: Applicative m => Applicative (AppT m)
derive newtype instance appTBind :: Bind m => Bind (AppT m)
derive newtype instance appTMonad :: Monad m => Monad (AppT m)

instance consoleLogMonad :: MonadEffect m => MonadLogger (AppT m) where
    log = EC.log >>> liftEffect >>> AppT
