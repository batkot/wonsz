module SignIn (component) where

import Prelude

import Assets (Assets)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as A
import Data.Tuple (Tuple(..), fst, snd)
import Dict (Dict)
import HTML.Components (spinner)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
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
    | RequestSignIn 
    | KeyDown String

canSignIn :: State -> Boolean
canSignIn state =
    state.username /= "" && state.password /= "" && not state.inProgress

condClasses :: forall r i. Array (Tuple HH.ClassName Boolean) -> HH.IProp (class :: String | r) i
condClasses = 
    HP.classes <<< map fst <<< A.filter snd

component :: forall q i o m. Dict -> Assets -> H.Component q i o m
component dict assets = 
    H.mkComponent 
        { initialState: \_ -> emptyState
        , render: render dict assets
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }

render :: forall m. Dict -> Assets -> State -> H.ComponentHTML Action () m
render dict assets state =
    HH.div
        [ HP.class_ (HH.ClassName "login-page") ]
        [ HH.div 
            [ HP.class_ (HH.ClassName "login-component") ]
            [ HH.div
                [ HP.class_ (HH.ClassName "logo") ]
                [ HH.img [HP.src assets.logoUrl] ]

            , HH.input
                [ HP.type_ InputText 
                , HP.placeholder dict.loginPlaceholder
                , HP.value state.username
                , HP.disabled state.inProgress
                , HE.onValueInput UsernameChanged
                , HE.onKeyDown (code >>> KeyDown)
                ]
            , HH.input
                [ HP.type_ InputPassword
                , HP.placeholder dict.passwordPlaceholder
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
                    , (Tuple (HH.ClassName "enabled") (canSignIn state))
                    , (Tuple (HH.ClassName "in-progress") state.inProgress)
                    ]
                , HE.onClick (\_ -> RequestSignIn)
                ]
                [ logInBtnContent ]
            ]
        ]
    where
        logInBtnContent = if state.inProgress 
            then spinner assets
            else HH.text dict.loginAction

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction (UsernameChanged  newUsername) =
    H.modify_ $ \st -> st { username = newUsername }

handleAction (PasswordChanged  newPassword) = do
    H.modify_ $ \st -> st { password = newPassword }

handleAction RequestSignIn = do
    loginAllowed <- H.gets canSignIn
    if loginAllowed 
        then H.modify_ $ \st -> st { inProgress = true }
        else pure unit

handleAction (KeyDown keyCode) = do
    if keyCode == "Enter" 
        then handleAction RequestSignIn 
        else pure unit
