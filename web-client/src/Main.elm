module Main exposing (main)

import Browser

import Auth 
import Login

import Session

import IO.LocalStorage as LS

import Html exposing (Html, text, img, div)
import Html.Attributes exposing (src, class)
import Html.Events exposing (onClick)

import Cmd.Extra as CE
import Http.Extra as HE

import Assets exposing (elmLogoUrl)

main : Program Options AppModel Command
main = 
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions 
        }

type alias Options =
    { apiUrl : String
    , authToken : Maybe String
    }

type alias Environment = HE.HasBaseUrl (Session.HasSessionSettings {})

init : Options -> (AppModel, Cmd Command)
init opt = 
    let env = createEnv opt
        model = Anonymous Login.emptyModel
        cmd = Maybe.map (Session.ValidateToken >> Session) opt.authToken
            |> Maybe.map CE.pure
            |> Maybe.withDefault Cmd.none
    in (AppModel env model, cmd)

createEnv : Options -> Environment
createEnv opt = 
    { baseUrl = HE.Url opt.apiUrl
    , sessionSettings = Session.SessionSettings "Session" 1
    }

update : Command -> AppModel -> (AppModel, Cmd Command)
update cmd app = 
    case (cmd, app.model) of 
        (Login loginCmd, Anonymous loginData) -> 
                let (model, c) = case Login.update app.env loginCmd loginData of
                        (Login.InProgress m, x) -> 
                            (m, Cmd.map Login x)
                        (Login.TokenObtained token, x) -> 
                            (loginData, Cmd.batch [ Cmd.map Login x, CE.pure (Session (Session.ValidateToken token))])
                in ({ app | model = Anonymous model }, c)

        (Session sessionCmd, m) ->
                let session = modelToSession m
                    (newSession, newCmd) = Session.update app.env sessionCmd session
                    newModel = updateSession m newSession
                in ({ app | model = newModel }, Cmd.map Session newCmd )

        (_, _) -> (app, Cmd.none)

modelToSession : Model -> Session.Session
modelToSession model = 
    case model of
        Anonymous _ -> Session.Anonymous
        Authorized m -> Session.Authenticated m.authSession

updateSession : Model -> Session.Session -> Model
updateSession model session = 
    case (model, session) of
        (Anonymous l, Session.Anonymous) -> Anonymous l
        (Authorized _, Session.Anonymous) -> Anonymous Login.emptyModel
        (Anonymous _, Session.Authenticated s) -> Authorized (AuthorizedModel s)
        (Authorized m, Session.Authenticated s) -> Authorized { m | authSession = s }

type alias AppModel = 
    { env : Environment
    , model : Model
    }

type Model 
    = Authorized AuthorizedModel 
    | Anonymous Login.LoginData

type alias AuthorizedModel = 
    { authSession : Auth.AuthSession
    }

type Command 
    = Login Login.LoginCmd
    | Session Session.Command

subscriptions : AppModel -> Sub Command
subscriptions _ = 
    LS.listenStringStorageKeyChange "Session"
    |> Sub.map (Maybe.map Session.ValidateToken >> Maybe.withDefault Session.Logout)
    |> Sub.map Session

view : AppModel -> Html Command
view app = 
    case app.model of
        Anonymous loginData -> Html.map Login (Login.view loginData)
        Authorized authorized -> loggedView authorized

loggedView : AuthorizedModel -> Html Command
loggedView  model = div 
    [ class "elm-container" ]
    [ text ((Auth.user >> Auth.userName) model.authSession)
    , img [ src elmLogoUrl ] []
    , text "Elm 0.19 Webpack4 Starter" 
    , div 
        [ onClick (Session Session.Logout) ]
        [ text "Logout" ]
    ]
