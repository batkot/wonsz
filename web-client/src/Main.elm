module Main exposing (main)

import Browser

import IO.Api as Api
import IO.LocalStorage as LS

import Auth 
import Login

import Html exposing (Html, text, img, div)
import Html.Attributes exposing (src, class)
import Html.Events exposing (onClick)

import Http 
import Http.Extra as HE

import Assets exposing (elmLogoUrl)

main : Program Options AppModel Command
main = 
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
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
    let authSession = Maybe.andThen Auth.parseToken opt.authToken
        model = case authSession of 
            Nothing -> Anonymous Login.emptyModel
            Just session -> Authorized <| AuthorizedModel session
        env = createEnv opt
    in (AppModel env model, Cmd.none)

createEnv : Options -> Environment
createEnv opt = { apiUrl = HE.Url opt.apiUrl }

update : Command -> AppModel -> (AppModel, Cmd Command)
update cmd app = 
    case (cmd, app.model) of 
        (Login loginCmd, Anonymous loginData) -> 
            case loginCmd of 
                Login.LoggedIn authToken -> 
                    let authResult = authenticate authToken
                    in  case authResult of
                        Nothing -> (app, Cmd.none)
                        Just (session, eff) -> 
                            ( { app | model = Authorized (AuthorizedModel session) } 
                            , Cmd.batch [ eff, testRenewToken app.env session] )

                _ -> Login.update app.env.apiUrl loginCmd loginData
                    |> Tuple.mapBoth Anonymous (Cmd.map Login)
                    |> Tuple.mapFirst (\m -> { app | model = m })
        (Logout, Authorized authModel) ->
            ( { app | model = Anonymous Login.emptyModel } , logout authModel.authSession )
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
    | Command (Result Http.Error Api.OverviewDto)
    | Logout

testRenewToken : HasApiUrl a -> Auth.AuthSession -> Cmd Command
testRenewToken { apiUrl } auth =
    Api.renewToken auth
    |> HE.mapRequest (\_ -> Api.OverviewDto "A")
    |> HE.execute apiUrl
    |> Cmd.map Command

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

authenticate : String -> Maybe (Auth.AuthSession, Cmd a)
authenticate tokenString = 
    Auth.parseToken tokenString
    |> Maybe.map (\ses -> (ses, Cmd.batch [ LS.storeString "AuthToken" tokenString ] ))

logout : Auth.Requires Auth.AuthSession (Cmd a)
logout = always <| LS.clearKey "AuthToken"
