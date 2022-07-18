module Root 
    ( Query(..)
    , component
    ) where

import Prelude

import Assets (Assets)
import Data.Maybe (Maybe(..))
import Dict (Dict)
import Effects.SignInMonad (class SignInMonad)
import Effects.Navigation (class NavigationMonad)
import Halogen as H
import Halogen.HTML as HH
import Routes (Route(..))
import SignIn as SignIn
import Dashboard as Dashboard
import Type.Proxy (Proxy(..))

data AuthSession
    = Anonymous
    | Authenticated String

data Query a = Navigate Route a

type State =
    { route :: Route
    , session :: AuthSession
    }

type Slots =
    ( signIn :: forall q o. H.Slot q o Unit
    , dashboard :: forall q o. H.Slot q o Unit
    )

_signIn = Proxy :: Proxy "signIn"
_dashboard = Proxy :: Proxy "dashboard"

type Action = Void

component 
    :: forall m
     . SignInMonad m
    => NavigationMonad m
    => Route
    -> Dict
    -> Assets
    -> H.Component Query Unit Void m
component startRoute dict assets =
    H.mkComponent
        { initialState: \_ -> { route: startRoute, session: Anonymous}
        , render: renderRoot
        , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
        }
  where
    renderRoot :: State -> H.ComponentHTML Action Slots m
    renderRoot state = 
        case state.session, state.route of 
            Anonymous, _ -> 
                HH.slot_ _signIn unit (SignIn.component dict assets) unit
            _, Dashboard -> 
                HH.slot_ _dashboard unit Dashboard.component unit
            _, (NotFound _) ->
                HH.slot_ _signIn unit (SignIn.component dict assets) unit
        
handleQuery :: forall a m. SignInMonad m => Query a -> H.HalogenM State Action Slots Void m (Maybe a)
handleQuery (Navigate newRoute x) = do
    H.modify_ $ \st -> st { route = newRoute }
    pure $ Just x
