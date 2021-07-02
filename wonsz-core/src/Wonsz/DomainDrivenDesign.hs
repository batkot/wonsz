{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Wonsz.DomainDrivenDesign 
    ( Aggregate(..)
    , AggregateAction
    , runAggregateAction

    , raiseEvent
    , throwError
    ) where

import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Except (Except, throwE, runExcept)
import Control.Monad.Trans.Writer.Strict (WriterT, tell, runWriterT)

class Aggregate st ev err | st -> ev, st -> err

newtype AggregateAction ev err st = AggregateAction 
    { unAggregateAction :: WriterT [ev] (Except err) st
    } deriving newtype (Functor, Applicative, Monad)

runAggregateAction = runExcept . runWriterT . unAggregateAction

raiseEvent :: ev -> AggregateAction ev err ()
raiseEvent ev = AggregateAction . tell $ [ev]

throwError :: err -> AggregateAction ev err ()
throwError = AggregateAction . lift . throwE
