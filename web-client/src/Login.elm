module Login exposing 
    ( LoginCmd(..)
    , LoginData
    , emptyModel
    , update
    , view
    )

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class, placeholder, type_, value, style)
import Html.Events exposing (onInput, onClick)

import Http
import Task
import Json.Encode as JE
import Json.Decode as JD

type LoginCmd 
    = Internal LoginCmdInternal
    | LoggedIn String

type LoginCmdInternal 
    = UserNameChanged String
    | PasswordChanged String
    | LoginRequested 
    | LoginResult (Result Http.Error String)

type alias LoginData =
    { user : String
    , password : String
    , failed : Bool
    }

encodeLoginData : LoginData -> JE.Value
encodeLoginData loginData = 
    JE.object
        [ ("user", JE.string loginData.user)
        , ("password", JE.string loginData.password)
        ]

type alias LoginUrl = String

emptyModel : LoginData
emptyModel = LoginData "" "" False

update : LoginUrl -> LoginCmd -> LoginData -> (LoginData, Cmd LoginCmd)
update loginUrl cmd model = 
    case cmd of 
        Internal internal -> updateInternal loginUrl internal model
        LoggedIn _ -> ( model, Cmd.none)

updateInternal : LoginUrl -> LoginCmdInternal -> LoginData -> (LoginData, Cmd LoginCmd)
updateInternal loginUrl cmd model =
    case cmd of
        UserNameChanged userName -> ( { model | user = userName }, Cmd.none )
        PasswordChanged password -> ( { model | password = password }, Cmd.none )
        LoginRequested -> (model, requestLogin loginUrl model)
        LoginResult (Ok authToken) -> (model, Task.perform LoggedIn (Task.succeed authToken))
        LoginResult (Err _) -> ({ model | failed = True }, Cmd.none)

requestLogin : LoginUrl -> LoginData -> Cmd LoginCmd
requestLogin loginUrl loginData = 
    Http.post
        { url = loginUrl
        , body = Http.jsonBody (encodeLoginData loginData)
        , expect = Http.expectJson (LoginResult >> Internal) JD.string
        }

view : LoginData -> Html LoginCmd
view loginData = 
    let
        error = if loginData.failed then div [] [text "Login failed!"] else div [] [] 
    in
        div [ class "login-form" ]
            [ input 
                [ type_ "text"
                , style "display" "block"
                , placeholder "Username"
                , value loginData.user
                , onInput (UserNameChanged >> Internal) ]
                []
            , input 
                [ type_ "password"
                , style "display" "block"
                , placeholder "Password"
                , value loginData.password
                , onInput (PasswordChanged >> Internal) ] 
                []
            , div 
                [ class "login-btn"
                , onClick (Internal LoginRequested)]
                [ text "Login" ]
            , error
            ]
