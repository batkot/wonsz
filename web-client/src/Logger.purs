module Logger 
    where

import Prelude

class Monad m <= MonadLogger m where
    log :: String -> m Unit
