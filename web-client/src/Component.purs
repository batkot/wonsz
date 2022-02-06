module Component (component) where

import Prelude

import Assets (Assets)

import Logger (class MonadLogger, log)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State = 
    { greeting :: String
    , logoUrl :: String
    }

type Action = Void

component :: forall q i o m. MonadLogger m => Assets -> String -> H.Component q i o m
component assets greeting = 
    H.mkComponent 
        { initialState: \_ -> { greeting: greeting, logoUrl: assets.purescriptLogoUrl }
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }

render :: forall m. State -> H.ComponentHTML Action () m
render state = 
    HH.div
        [ HP.class_ (HH.ClassName "greet-component") ]
        [ HH.div_ [ HH.text state.greeting ]
        , HH.img [HP.src state.logoUrl]
        , HH.div_ [ HH.text "Purescript Halogen Webpack" ]
        ]

handleAction :: forall o m. MonadLogger m => Void -> H.HalogenM State Action () o m Unit
handleAction _ = H.lift $ log "Some logger"
