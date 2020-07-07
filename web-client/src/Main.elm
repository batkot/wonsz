module Main exposing (main)

import Browser

import Auth 
import Login

import IO.LocalStorage as LS
import IO.Api as Api

import Html exposing (Html, text, img, div)
import Html.Attributes exposing (src, class)
import Html.Events exposing (onClick)

import Cmd.Extra as CE
import Http.Extra as HE
import Delay as D
import Result.Extra as RE

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

type alias Environment = HasApiUrl {}

type alias HasApiUrl a =
    { a | apiUrl : HE.Url}

init : Options -> (AppModel, Cmd Command)
init opt = 
    let env = createEnv opt
        model = Anonymous Login.emptyModel
        cmd = Maybe.map AuthTokenChanged opt.authToken
            |> Maybe.map CE.pure
            |> Maybe.withDefault Cmd.none
    in (AppModel env model, cmd)

createEnv : Options -> Environment
createEnv opt = { apiUrl = HE.Url opt.apiUrl }

update : Command -> AppModel -> (AppModel, Cmd Command)
update cmd app = 
    case (cmd, app.model) of 
        (Login loginCmd, Anonymous loginData) -> 
                let (model, c) = Login.update app.env.apiUrl loginCmd loginData
                        |> Tuple.mapBoth Anonymous (Cmd.map Login)
                in ({ app | model = model }, c)

        (Logout, Authorized authModel) ->
            ( { app | model = Anonymous Login.emptyModel } , Auth.logout authModel.authSession )

        (AuthTokenChanged newToken, Authorized authModel) -> 
            let (newModel, newCmd) = case Auth.authenticate newToken of 
                    Nothing -> 
                        ( Anonymous Login.emptyModel, Auth.logout authModel.authSession)
                    Just (authSession, c) -> 
                        ( Authorized { authModel | authSession = authSession }
                        , Cmd.batch
                            [ c
                            , D.after 1 D.Hour (RenewAuthToken authSession)
                            ]
                        )
            in ( { app | model = newModel }, newCmd )
        (AuthTokenChanged newToken, Anonymous x) -> 
            let (newModel, newCmd) = case Auth.authenticate newToken of
                    Nothing -> (Anonymous x, Cmd.none)
                    Just (authSession, c) -> 
                        ( Authorized (AuthorizedModel authSession)
                        , Cmd.batch
                            [ c
                            , D.after 1 D.Hour (RenewAuthToken authSession)
                            ]
                        )
            in ( { app | model = newModel }, newCmd )
        (RenewAuthToken _, Authorized authModel) ->
            let c = Api.renewToken authModel.authSession
                    |> HE.execute app.env.apiUrl
                    |> Cmd.map (RE.unpack (\_ -> Logout) AuthTokenChanged)
            in (app, c)

        (_, _) -> (app, Cmd.none)

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
    | Logout
    | AuthTokenChanged Auth.TokenString
    | RenewAuthToken Auth.AuthSession
    | NoOp -- :( 

subscriptions : AppModel -> Sub Command
subscriptions _ = 
    LS.listenStringStorageKeyChange "AuthToken" 
    |> Sub.map (Maybe.map AuthTokenChanged >> Maybe.withDefault NoOp)


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
        [ onClick Logout ]
        [ text "Logout" ]
    ]
