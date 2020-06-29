module Main exposing (main)

import Browser

import LocalStorage as LS
import Login

import Html exposing (Html, text, img, div)
import Html.Attributes exposing (src, class)

import Assets exposing (elmLogoUrl)

main : Program Environment AppModel Command
main = 
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }

type alias Environment =
    { apiUrl : String
    , authToken : Maybe String
    }

init : Environment -> (AppModel, Cmd Command)
init env = 
    let model = case env.authToken of 
            Nothing -> Anonymous Login.emptyModel
            Just token -> Authorized <| AuthorizedModel token
    in (AppModel env model, Cmd.none)

update : Command -> AppModel -> (AppModel, Cmd Command)
update cmd app = 
    case (cmd, app.model) of 
        (Login loginCmd, Anonymous loginData) -> 
            case loginCmd of 
                Login.LoggedIn authToken -> 
                    ( { app | model = Authorized (AuthorizedModel authToken) }
                    , LS.storeString "AuthToken" authToken)
                _ -> Login.update app.env.apiUrl loginCmd loginData
                    |> Tuple.mapBoth Anonymous (Cmd.map Login)
                    |> Tuple.mapFirst (\m -> { app | model = m })
        (_, _) -> (app, Cmd.none)

type alias AppModel = 
    { env : Environment
    , model : Model
    }

type Model 
    = Authorized AuthorizedModel 
    | Anonymous Login.LoginData

type alias AuthorizedModel = 
    { authToken : String
    }

type Command 
    = Login Login.LoginCmd
    | Command

view : AppModel -> Html Command
view app = 
    case app.model of
        Anonymous loginData -> Html.map Login (Login.view loginData)
        Authorized authorized -> loggedView authorized

loggedView : AuthorizedModel -> Html a
loggedView  model = div 
    [ class "elm-container" ]
    [ text model.authToken
    , img [ src elmLogoUrl ] []
    , text "Elm 0.19 Webpack4 Starter" 
    ]
