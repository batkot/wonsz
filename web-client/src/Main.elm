module Main exposing (main)

import Browser

import IO.Api as Api

import Auth 
import LocalStorage as LS
import Login

import Html exposing (Html, text, img, div)
import Html.Attributes exposing (src, class)

import Http 
import Http.Extra as HE

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
    let authSession = Maybe.andThen Auth.parseToken env.authToken
        model = case authSession of 
            Nothing -> Anonymous Login.emptyModel
            Just session -> Authorized <| AuthorizedModel session
    in (AppModel env model, Cmd.none)

update : Command -> AppModel -> (AppModel, Cmd Command)
update cmd app = 
    case (cmd, app.model) of 
        (Login loginCmd, Anonymous loginData) -> 
            case loginCmd of 
                Login.LoggedIn authToken -> 
                    let authSession = Auth.parseToken authToken
                    in  case authSession of
                        Nothing -> (app, Cmd.none)
                        Just session -> ( { app | model = Authorized (AuthorizedModel session) }
                                        , Cmd.batch
                                            [ LS.storeString "AuthToken" authToken
                                            , authHttpTest session
                                            ])

                _ -> Login.update loginCmd loginData
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
    { authSession : Auth.AuthSession
    }

makeApiUrl : Environment -> String -> String
makeApiUrl env path = env.apiUrl ++ path

-- This goes to separate module with Auth info?
authHttpTest : Auth.AuthSession -> Cmd Command
authHttpTest session =
    Api.overview session
    |> HE.execute
    |> Cmd.map Command

type Command 
    = Login Login.LoginCmd
    | Command (Result Http.Error Api.OverviewDto)

view : AppModel -> Html Command
view app = 
    case app.model of
        Anonymous loginData -> Html.map Login (Login.view loginData)
        Authorized authorized -> loggedView authorized

loggedView : AuthorizedModel -> Html a
loggedView  model = div 
    [ class "elm-container" ]
    [ text ((Auth.user >> Auth.userName) model.authSession)
    , img [ src elmLogoUrl ] []
    , text "Elm 0.19 Webpack4 Starter" 
    ]
