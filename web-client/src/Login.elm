module Login exposing 
    ( LoginCmd
    , LoginData

    , emptyModel
    , update
    , view

    , redirectRoute
    )

import Html exposing (Html, div, input, text, img)
import Html.Attributes exposing (class, placeholder, type_, value, src)
import Html.Events exposing (onInput, onClick)
import Html.Extra exposing (enter, onKey)

import IO.Api as Api

import Effect as Fx exposing (Fx)
import Effect.Http exposing (HttpFx(..))
import Effect.Compose as Fx exposing (FxComp) 
import Effect.AuthenticationToken exposing (AuthenticationTokenFx, raiseTokenAquired)

import Http
import Http.Extra as HE

import Assets 

import Router.Routes exposing (Route)

import Lang exposing (HasDict)

type alias LoginFx = FxComp (HttpFx LoginCmd) (AuthenticationTokenFx String)

type LoginCmd
    = UserNameChanged String
    | PasswordChanged String
    | RequestLogin 
    | LoginFailed LoginError
    | LoggedIn String

type alias LoginData =
    { user : String
    , password : String
    , redirectToRoute : Route
    , error : Maybe LoginError
    }

redirectRoute : LoginData -> Route
redirectRoute { redirectToRoute } = redirectToRoute

type LoginError 
    = ConnectionError
    | BadCredentials
    | Other String

emptyModel : Route -> LoginData
emptyModel redirectToRoute = LoginData "" "" redirectToRoute Nothing

update : LoginCmd -> LoginData -> Fx LoginFx LoginData
update cmd model =
    case cmd of
        UserNameChanged userName -> 
            Fx.pure { model | user = userName }
        PasswordChanged password -> 
            Fx.pure { model | password = password }
        RequestLogin -> 
            Fx.pure model
            |> Fx.pushLeft (requestLoginFx model)
        LoginFailed err -> 
            Fx.pure { model | error = Just err }
        LoggedIn token -> 
            Fx.pure model
            |> Fx.pushRight (raiseTokenAquired token)

requestLoginFx : LoginData -> HttpFx LoginCmd
requestLoginFx loginData =
    let errorMap err = case err of
            Http.BadStatus s -> if s == 401 then BadCredentials else Other (HE.httpErrorToMessage (Http.BadStatus s))
            Http.Timeout -> ConnectionError
            Http.NetworkError -> ConnectionError
            x -> Other (HE.httpErrorToMessage x) 

        apiCall = Api.login loginData.user loginData.password |> HE.mapRequest LoggedIn
    in Request apiCall (errorMap >> LoginFailed)

view : HasDict a -> LoginData -> Html LoginCmd
view { dict } loginData = 
    let
        loginErrorText = case loginData.error of
            (Just BadCredentials) -> dict.badCredentialsMessage
            (Just ConnectionError) -> dict.connectionErrorMessage
            (Just (Other msg)) -> msg
            Nothing -> ""
    in
        div 
        [ class "login-container" ] 
        [ div 
            [ class "login-form" ]
            [ div 
                [ class "title" ]
                [ img [ src Assets.logo ] [] ]
            , div [] 
                [ input 
                    [ type_ "text"
                    , placeholder dict.loginPlaceholder
                    , value loginData.user
                    , onInput UserNameChanged
                    , onKey enter RequestLogin ] 
                    []
                ]
            , div [] 
                [ input 
                    [ type_ "password"
                    , placeholder dict.passwordPlaceholder
                    , value loginData.password
                    , onInput PasswordChanged
                    , onKey enter RequestLogin ] 
                    []
                ]
            , div 
                [ class "login-error" ]
                [ text loginErrorText ]
            , div 
                [ class "login-btn"
                , onClick RequestLogin ]
                [ text dict.loginAction ]
            ]
        ]
