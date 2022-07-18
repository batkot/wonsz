module SignIn (component) where

import Prelude

import Assets (Assets)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as A
import Data.Either (either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Dict (Dict)
import Effects.Navigation (class NavigationMonad, navigate)
import Effects.SignInMonad (class SignInMonad, signIn)
import HTML.Components (spinner)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Routes as R
import Web.UIEvent.KeyboardEvent (code)

type State = 
    { username :: String
    , password :: String
    , inProgress :: Boolean
    , error :: Maybe String
    }

emptyState :: State
emptyState =
    { username: ""
    , password: ""
    , inProgress: false
    , error: Nothing
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

component 
    :: forall q i o m
     . SignInMonad m 
    => NavigationMonad m
    => Dict 
    -> Assets 
    -> H.Component q i o m
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
                [ HH.text $ fromMaybe "" state.error ]
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

handleAction 
    :: forall o m
     . SignInMonad m 
    => NavigationMonad m
    => Action 
    -> H.HalogenM State Action () o m Unit

handleAction (UsernameChanged  newUsername) =
    H.modify_ $ \st -> st { username = newUsername }

handleAction (PasswordChanged  newPassword) = do
    H.modify_ $ \st -> st { password = newPassword }

handleAction RequestSignIn = do
    state <- H.get
    when (canSignIn state) $ do
        H.modify_ $ \st -> st { inProgress = true }
        signInResult <- H.lift $ signIn { username: state.username, password: state.password }
        H.lift $ navigate R.Dashboard
        --H.modify_ $ \st -> st { inProgress = false, error = either (\_ -> Just "Invalid Credentials") (\_ -> Nothing) signInResult }

handleAction (KeyDown keyCode) = do
    if keyCode == "Enter" 
        then handleAction RequestSignIn 
        else pure unit
