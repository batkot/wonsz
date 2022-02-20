module Root 
    ( Query(..)
    , component
    ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effects.SignInMonad (class SignInMonad)
import Halogen as H
import Halogen.HTML as HH
import Routes (Route)
import SignIn as SignIn
import Type.Proxy (Proxy(..))
import Dict (Dict)
import Assets (Assets)

data Query a = Navigate Route a

type State =
    { route :: Route
    }

type Slots =
    ( signIn :: forall q o. H.Slot q o Int
    )

_signIn = Proxy :: Proxy "signIn"

type Action = Void

component 
    :: forall m
     . SignInMonad m
    => Route
    -> Dict
    -> Assets
    -> H.Component Query Unit Void m
component startRoute dict assets =
    H.mkComponent
        { initialState: \_ -> { route: startRoute }
        , render: renderRoot
        , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
        }
  where
    renderRoot :: State -> H.ComponentHTML Action Slots m
    renderRoot _ = HH.slot_ _signIn 0 (SignIn.component dict assets) unit

handleQuery :: forall a m. SignInMonad m => Query a -> H.HalogenM State Action Slots Void m (Maybe a)
handleQuery (Navigate newRoute x) = do
    H.modify_ $ \st -> st { route = newRoute }
    pure $ Just x
