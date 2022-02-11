module Dict
    where

import Prelude

type Label = String

type Dict = 
    { loginPlaceholder :: Label
    , passwordPlaceholder :: Label
    , loginAction :: Label
    }

class Monad m <= MonadDict m where
    getDict :: m Dict

