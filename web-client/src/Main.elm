module Main exposing (main)

import Browser

import Html exposing (Html, div, text, span, img)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)

import Assets

import Effect as Fx
import Effect.Compose as Fx
import Effect.Command as FxC exposing (runCommandFx)
import Effect.Http as FxH exposing (runHttpFx)
import Effect.LocalStorage exposing (runLocalStorageFx)
import Effect.AuthenticationToken as FxAT exposing (runAuthenticationTokenFx)

import Session as S
import Auth exposing (AuthSession)

import IO.LocalStorage as LS

import Cmd.Extra as CE
import Http.Extra as HE

import Lang
import Lang.Pl as PL
import Lang.En as EN
import Router as R

import Url 
import Browser.Navigation as Nav

import Router.Routes as RR
import Page as P 

main : Program Options AppModel Command
main = 
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions 
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        }

onUrlChange : Url.Url -> Command
onUrlChange = RR.parseUrl >> P.requestPage >> PageCmd

onUrlRequest : Browser.UrlRequest -> Command
onUrlRequest = R.RouteRequested >> RoutingCmd

type alias Options =
    { apiUrl : String
    , authToken : Maybe String
    , sessionCacheKey : String
    , lang : String
    }

type alias Environment = 
    Lang.HasDict (HE.HasBaseUrl (S.HasSessionSettings (R.HasNavKey {})))

init : Options -> Url.Url -> Nav.Key -> (AppModel, Cmd Command)
init opt url key = 
    let env = createEnv opt key
        model = RR.parseUrl url
            |> P.requireLogin 
        cmd = Maybe.map (S.ValidateToken >> SessionCmd) opt.authToken
            |> Maybe.map CE.pure
            |> Maybe.withDefault Cmd.none
    in (AppModel env S.Anonymous model, cmd)

createEnv : Options -> Nav.Key -> Environment
createEnv opt navKey = 
    let
        dict = case opt.lang of 
            "pl" -> PL.dictionary
            _ -> EN.dictionary
    in
        { baseUrl = HE.Url opt.apiUrl
        , sessionSettings = S.SessionSettings opt.sessionCacheKey 59
        , dict = dict
        , navKey = navKey
        }

update : Command -> AppModel -> (AppModel, Cmd Command)
update cmd app = 
    case cmd of 
        (SessionCmd sessionCmd) ->
            let session = P.pageSession app.currentPage
                fxSession = S.updateFx app.env sessionCmd session
                fxInterpreter = 
                    Fx.runFxComp runLocalStorageFx (runHttpFx app.env.baseUrl)
                    |> Fx.runFxComp runCommandFx 
                (newSession, newSessionCmd) = Fx.runFx fxInterpreter fxSession
                (newPageModel, pageCmd) = 
                    P.updateSession newSession app.currentPage
                    |> Fx.runFx runCommandFx
                newApp = { app | currentPage = newPageModel, session = newSession }
                newCmd = Cmd.batch [ Cmd.map SessionCmd newSessionCmd, Cmd.map PageCmd pageCmd]
            in 
                (newApp, newCmd)

        (RoutingCmd routingCmd) -> 
                ( app, R.handleRouting app.env routingCmd )

        (PageCmd pageCmd) ->
            let fxPage = 
                    P.update pageCmd app.currentPage
                    |> Fx.mapFx (Fx.mapLeft (FxC.map PageCmd))
                    |> Fx.mapFx (Fx.mapRight (Fx.bimap (FxH.map PageCmd) (FxAT.map (S.ValidateToken >> SessionCmd))))
                fxInterpreter =
                    Fx.runFxComp (runHttpFx app.env.baseUrl) runAuthenticationTokenFx
                    |> Fx.runFxComp runCommandFx
                (newPageModel, newCmd) = Fx.runFx fxInterpreter fxPage
            in ({ app | currentPage = newPageModel }, newCmd)

type alias AppModel = 
    { env : Environment
    , session : S.Session
    , currentPage : P.PageModel
    }

type Command 
    = PageCmd P.PageCommand
    | SessionCmd S.Command
    | RoutingCmd R.Command

subscriptions : AppModel -> Sub Command
subscriptions model = 
    LS.listenStringStorageKeyChange model.env.sessionSettings.cacheKey
    |> Sub.map (Maybe.map S.ValidateToken >> Maybe.withDefault S.Logout)
    |> Sub.map SessionCmd

view : AppModel -> Browser.Document Command
view app = 
    let { title, html } = P.view app.env  app.currentPage |> P.pageViewMap PageCmd
        pageView = case app.session of
            S.Anonymous 
                -> html
            S.Authenticated auth 
                -> loggedView app.env auth html
    in { title = title
       , body = [pageView]
       }
    
loggedView : Lang.HasDict a -> AuthSession -> Html Command -> Html Command
loggedView d auth subContent = 
    let
        header = div 
            [ class "header" ]
            [ img [ src Assets.logo ] [] 
            , div 
                [ class "logged-user" ]
                [ span 
                    [ class "username" ]
                    [ text ((Auth.user >> Auth.userName) auth) ]
                , span 
                    [ class "logout-btn" 
                    , onClick (SessionCmd S.Logout) 
                    ]
                    [ text d.dict.logoutAction ] 
                ]
            ]
        content = div [ class "content" ] [ subContent ]
    in
        div [ class "logged-container" ]
            [ header
            , content
            ]
