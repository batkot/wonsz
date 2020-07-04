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

import IO.Api as Api
import Http.Extra as HE

type LoginCmd 
    = Internal LoginCmdInternal
    | LoggedIn String

type LoginCmdInternal 
    = UserNameChanged String
    | PasswordChanged String
    | RequestLogin 
    | LoginFailed String

type alias LoginData =
    { user : String
    , password : String
    , failed : Bool
    }

emptyModel : LoginData
emptyModel = LoginData "" "" False

update : HE.Url -> LoginCmd -> LoginData -> (LoginData, Cmd LoginCmd)
update apiUrl cmd model = 
    case cmd of 
        Internal internal -> updateInternal apiUrl internal model
        LoggedIn _ -> ( model, Cmd.none)

updateInternal : HE.Url -> LoginCmdInternal -> LoginData -> (LoginData, Cmd LoginCmd)
updateInternal apiUrl cmd model =
    case cmd of
        UserNameChanged userName -> ( { model | user = userName }, Cmd.none )
        PasswordChanged password -> ( { model | password = password }, Cmd.none )
        RequestLogin -> (model, requestLogin apiUrl model)
        LoginFailed _ -> ({ model | failed = True }, Cmd.none)

requestLogin : HE.Url -> LoginData -> Cmd LoginCmd
requestLogin apiUrl loginData = 
    Api.login loginData.user loginData.password
    |> HE.execute apiUrl
    |> Cmd.map (Result.map LoggedIn >> Result.withDefault (Internal (LoginFailed "Dupa")))

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
                , onClick (Internal RequestLogin)]
                [ text "Login" ]
            , error
            ]
