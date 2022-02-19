module Effects.SignInMonad where

import Prelude
import Data.Either (Either)

type SignInRequest =
    { username :: String
    , password :: String
    }

data SignInError 
    = InvalidCredentials
    | Other

newtype SessionToken = SessionToken String

class Monad m <= SignInMonad m where
    signIn :: SignInRequest -> m (Either SignInError SessionToken)
