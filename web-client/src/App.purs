module App where

import Prelude

import Data.Either (Either(..))
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as EC
import Effects.SignInMonad (class SignInMonad, SessionToken(..))
import IO.Api as Api
import Logger (class MonadLogger)

type AppConfig =
    { apiUrl :: Api.ApiUrl
    }

newtype AppT :: forall k. (k -> Type) -> k -> Type
newtype AppT m a = AppT (ReaderT AppConfig m a)

runAppT :: forall m a. AppConfig -> AppT m a -> m a
runAppT config (AppT app) = runReaderT app config

derive newtype instance functorAppT :: Functor f => Functor (AppT f)
derive newtype instance applyAppT :: Apply m => Apply (AppT m)
derive newtype instance applicativeAppT :: Applicative m => Applicative (AppT m)
derive newtype instance bindAppT :: Bind m => Bind (AppT m)
derive newtype instance monadAppT :: Monad m => Monad (AppT m)

instance consoleLogMonadAppT :: MonadEffect m => MonadLogger (AppT m) where
    log = EC.log >>> liftEffect >>> AppT

instance signInMonadAppT :: MonadAff m => SignInMonad (AppT m) where
    signIn req = AppT $ do
       { apiUrl } <- ask
       -- liftAff $ Api.signIn apiUrl req
       pure $ Right $ SessionToken "Totally Legit Token"
