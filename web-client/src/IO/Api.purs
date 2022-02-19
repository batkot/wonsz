module IO.Api 
    ( ApiUrl(..)
    , signIn
    ) where

import Prelude

import Affjax as AX
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect.Aff (Aff)
import Effects.SignInMonad (SessionToken(..), SignInError(..), SignInRequest)

newtype ApiUrl  = ApiUrl String

signIn :: ApiUrl -> SignInRequest -> Aff (Either SignInError SessionToken)
signIn (ApiUrl apiUrl) _ = 
    bimap (\_ -> InvalidCredentials) (\_ -> SessionToken "Dupa") <$> AX.request req 
  where
    req = AX.defaultRequest 
        { url = apiUrl <> "/login"
        , method = Left POST }
