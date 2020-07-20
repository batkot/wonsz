module Main exposing (main)

import Browser

import Auth 
import Login
import Effect as Fx

import Session as S

import IO.LocalStorage as LS

import Html exposing (Html, text, img, div, span)
import Html.Attributes exposing (src, class)
import Html.Events exposing (onClick)

import Cmd.Extra as CE
import Http.Extra as HE

import Assets 
import Lang
import Lang.Pl as PL
import Lang.En as EN

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
    , sessionCacheKey : String
    , lang : String
    }

type alias Environment = Lang.HasDict (HE.HasBaseUrl (S.HasSessionSettings {}))

init : Options -> (AppModel, Cmd Command)
init opt = 
    let env = createEnv opt
        model = Anonymous Login.emptyModel
        cmd = Maybe.map (S.ValidateToken >> Session) opt.authToken
            |> Maybe.map CE.pure
            |> Maybe.withDefault Cmd.none
    in (AppModel env model, cmd)

createEnv : Options -> Environment
createEnv opt = 
    let
        dict = case opt.lang of 
            "pl" -> PL.dictionary
            _ -> EN.dictionary
    in
        { baseUrl = HE.Url opt.apiUrl
        , sessionSettings = S.SessionSettings opt.sessionCacheKey 1
        , dict = dict
        }

update : Command -> AppModel -> (AppModel, Cmd Command)
update cmd app = 
    case (cmd, app.model) of 
        (Login loginCmd, Anonymous loginData) -> 
                let (model, c) = case Login.update app.env loginCmd loginData of
                        (Login.InProgress m, x) -> 
                            (m, Cmd.map Login x)
                        (Login.TokenObtained token, x) -> 
                            (loginData, Cmd.batch [ Cmd.map Login x, CE.pure (Session (S.ValidateToken token))])
                in ({ app | model = Anonymous model }, c)

        (Session sessionCmd, m) ->
                let session = modelToSession m
                    fxSession = S.updateFx app.env sessionCmd session
                    fxInterpreter = 
                        Fx.runCompFx Fx.runLocalStorageFx (Fx.runHttpFx app.env.baseUrl)
                        |> Fx.runCompFx Fx.runCommandFx 
                    (newSession, newCmd) = Fx.interpret Cmd.none fxInterpreter fxSession
                    newModel = updateSession m newSession
                in ({ app | model = newModel }, Cmd.map Session newCmd )
                
        (_, _) -> (app, Cmd.none)

modelToSession : Model -> S.Session
modelToSession model = 
    case model of
        Anonymous _ -> S.Anonymous
        Authorized m -> S.Authenticated m.authSession

updateSession : Model -> S.Session -> Model
updateSession model session = 
    case (model, session) of
        (Anonymous l, S.Anonymous) -> Anonymous l
        (Authorized _, S.Anonymous) -> Anonymous Login.emptyModel
        (Anonymous _, S.Authenticated s) -> Authorized (AuthorizedModel s)
        (Authorized m, S.Authenticated s) -> Authorized { m | authSession = s }

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
    | Session S.Command

subscriptions : AppModel -> Sub Command
subscriptions model = 
    LS.listenStringStorageKeyChange model.env.sessionSettings.cacheKey
    |> Sub.map (Maybe.map S.ValidateToken >> Maybe.withDefault S.Logout)
    |> Sub.map Session

view : AppModel -> Html Command
view app = 
    case app.model of
        Anonymous loginData -> Html.map Login (Login.view app.env loginData)
        Authorized authorized -> loggedView authorized

loggedView : AuthorizedModel -> Html Command
loggedView model = 
    let
        header = div 
            [ class "header" ]
            [ img [ src Assets.logo ] [] 
            , span 
                [ onClick (Session S.Logout) ] 
                [ text ((Auth.user >> Auth.userName) model.authSession) ]
            ]
        content = div [ class "content" ] [ text "Hello" ]
    in
        div [ class "logged-container" ]
            [ header
            , content 
            ]
