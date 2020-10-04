module Login exposing 
    ( LoginCmd
    , LoginData
    , LoginResult(..)

    , emptyModel
    , update
    , view
    )

import Html exposing (Html, div, input, text, img)
import Html.Attributes exposing (class, placeholder, type_, value, src)
import Html.Events exposing (onInput, onClick)
import Html.Extra exposing (enter, onKey)

import Result.Extra as RE

import IO.Api as Api

import Http
import Http.Extra as HE

import Auth
import Assets 

import Lang exposing (HasDict)

type LoginCmd
    = UserNameChanged String
    | PasswordChanged String
    | RequestLogin 
    | LoginFailed LoginError
    | LoggedIn String

type alias LoginData =
    { user : String
    , password : String
    , error : Maybe LoginError
    }

type LoginError 
    = ConnectionError
    | BadCredentials
    | Other String

type LoginResult 
    = InProgress LoginData
    | TokenObtained Auth.TokenString

emptyModel : LoginData
emptyModel = LoginData "" "" Nothing

update : HE.HasBaseUrl a -> LoginCmd -> LoginData -> (LoginResult, Cmd LoginCmd)
update { baseUrl } cmd model =
    case cmd of
        UserNameChanged userName -> 
            ( InProgress { model | user = userName }, Cmd.none )
        PasswordChanged password -> 
            ( InProgress { model | password = password }, Cmd.none )
        RequestLogin -> 
            ( InProgress model, requestLogin baseUrl model)
        LoginFailed err -> 
            ( InProgress { model | error = Just err }, Cmd.none)
        LoggedIn token -> 
            ( TokenObtained token, Cmd.none)

requestLogin : HE.Url -> LoginData -> Cmd LoginCmd
requestLogin apiUrl loginData = 
    let errorMap err = case err of
            Http.BadStatus s -> if s == 401 then BadCredentials else Other (HE.httpErrorToMessage (Http.BadStatus s))
            Http.Timeout -> ConnectionError
            Http.NetworkError -> ConnectionError
            x -> Other (HE.httpErrorToMessage x)
    in Api.login loginData.user loginData.password
        |> HE.execute apiUrl
        |> Cmd.map (RE.unpack (errorMap >> LoginFailed) LoggedIn)

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
