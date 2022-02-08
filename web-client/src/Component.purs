module Component (component) where

import Data.Tuple
import Prelude

import Assets (Assets)
import Control.Monad.List.Trans (filter)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as A
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Logger (class MonadLogger, log)
import Web.UIEvent.KeyboardEvent (code)

type State = 
    { username :: String
    , password :: String
    , inProgress :: Boolean
    }

emptyState :: State
emptyState =
    { username: ""
    , password: ""
    , inProgress: false
    }

data Action 
    = UsernameChanged String
    | PasswordChanged String
    | RequestLogIn 
    | KeyDown String

canLogIn :: State -> Boolean
canLogIn state =
    state.username /= "" && state.password /= "" && not state.inProgress

condClasses :: forall r i. Array (Tuple HH.ClassName Boolean) -> HH.IProp (class :: String | r) i
condClasses = 
    HP.classes <<< map fst <<< A.filter snd

component :: forall q i o m. MonadLogger m => Assets -> String -> H.Component q i o m
component assets greeting = 
    H.mkComponent 
        { initialState: \_ -> emptyState
        , render: render assets.singleSnakeUrl assets.logoUrl
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }

render :: forall m. String -> String -> State -> H.ComponentHTML Action () m
render spinnerUrl logoUrl state = 
    HH.div
        [ HP.class_ (HH.ClassName "login-page") ]
        [ HH.div 
            [ HP.class_ (HH.ClassName "login-component") ]
            [ HH.div
                [ HP.class_ (HH.ClassName "logo") ]
                [ HH.img [HP.src logoUrl] ]

            , HH.input
                [ HP.type_ InputText 
                , HP.placeholder "Użytkownik"
                , HP.value state.username
                , HP.disabled state.inProgress
                , HE.onValueInput UsernameChanged
                , HE.onKeyDown (code >>> KeyDown)
                ]
            , HH.input
                [ HP.type_ InputPassword
                , HP.placeholder "Hasło"
                , HP.value state.password
                , HP.disabled state.inProgress
                , HE.onValueInput PasswordChanged
                , HE.onKeyDown (code >>> KeyDown)
                ]
            , HH.div
                [ HP.class_ (HH.ClassName "login-error")]
                []
            , HH.div
                [ condClasses 
                    [ (Tuple (HH.ClassName "login-btn") true)
                    , (Tuple (HH.ClassName "enabled") (canLogIn state))
                    , (Tuple (HH.ClassName "in-progress") state.inProgress)
                    ]
                , HE.onClick (\_ -> RequestLogIn)
                ]
                [ logInBtnContent ]
            ]
        ]
    where
      logInBtnContent = 
          if state.inProgress 
              then HH.img [ HP.src spinnerUrl ]
              else HH.text "Zaloguj"

handleAction :: forall o m. MonadLogger m => Action -> H.HalogenM State Action () o m Unit
handleAction (UsernameChanged  newUsername) =
    H.modify_ $ \st -> st { username = newUsername }

handleAction (PasswordChanged  newPassword) = do
    H.modify_ $ \st -> st { password = newPassword }

handleAction RequestLogIn =
    H.modify_ $ \st -> st { inProgress = true }

handleAction (KeyDown keyCode) = do
    state <- H.get
    if (keyCode == "Enter" && canLogIn state) 
        then handleAction RequestLogIn
        else pure unit

