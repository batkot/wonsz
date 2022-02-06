module Component (component) where

import Prelude

import Assets (Assets)
import DOM.HTML.Indexed.InputType (InputType(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Logger (class MonadLogger, log)

type State = 
    { greeting :: String
    , logoUrl :: String
    }

type Action = Void

component :: forall q i o m. MonadLogger m => Assets -> String -> H.Component q i o m
component assets greeting = 
    H.mkComponent 
        { initialState: \_ -> { greeting: greeting, logoUrl: assets.logoUrl }
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }

render :: forall m. State -> H.ComponentHTML Action () m
render state = 
    HH.div
        [ HP.class_ (HH.ClassName "login-page") ]
        [ HH.div 
            [ HP.class_ (HH.ClassName "login-component") ]
            [ HH.div
                [ HP.class_ (HH.ClassName "logo") ]
                [ HH.img [HP.src state.logoUrl] ]

            , HH.input
                [ HP.type_ InputText 
                , HP.placeholder "Użytkownik"
                ]
            , HH.input
                [ HP.type_ InputPassword
                , HP.placeholder "Hasło"
                ]
            , HH.div
                [ HP.class_ (HH.ClassName "login-error")]
                []
            , HH.div
                [ HP.class_ (HH.ClassName "login-btn")]
                [ HH.text "Zaloguj" ]
            ]
        ]

handleAction :: forall o m. MonadLogger m => Void -> H.HalogenM State Action () o m Unit
handleAction _ = H.lift $ log "Some logger"
