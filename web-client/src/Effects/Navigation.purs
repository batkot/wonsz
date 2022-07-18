module Effects.Navigation where

import Prelude

import Routes (Route)

class Monad m <= NavigationMonad m where
    navigate :: Route -> m Unit
