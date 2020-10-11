module Main exposing (main)

import Browser

import Auth 
import Login
import Effect as Fx
import Effect.Compose as Fx
import Effect.Command exposing (runCommandFx)
import Effect.Http as FxH exposing (runHttpFx)
import Effect.LocalStorage exposing (runLocalStorageFx)
import Effect.AuthenticationToken as FxAT exposing (runAuthenticationTokenFx)

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
import Router as R

import Url 
import Url.Builder as UB
import Browser.Navigation as Nav

import Scoreboard as SB

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
onUrlChange = always Noop
-- onUrlChange = R.UrlChanged >> Routing

onUrlRequest : Browser.UrlRequest -> Command
onUrlRequest = always Noop
-- onUrlRequest = R.RouteRequested >> Routing

type alias Options =
    { apiUrl : String
    , authToken : Maybe String
    , sessionCacheKey : String
    , lang : String
    }

type alias Environment = 
    Lang.HasDict (HE.HasBaseUrl (S.HasSessionSettings (R.HasRouter RR.Route P.PageModel P.PageCommand {})))

dispatchRoute : RR.Route -> (P.PageModel, Cmd P.PageCommand)
dispatchRoute _ = (P.requireLogin, Cmd.none)

mkRouter : Nav.Key -> R.Router RR.Route P.PageModel P.PageCommand 
mkRouter navKey = R.Router
    RR.routes
    dispatchRoute
    RR.NotFound
    navKey

init : Options -> Url.Url -> Nav.Key -> (AppModel, Cmd Command)
init opt url key = 
    let env = createEnv opt key
        model = P.requireLogin
        cmd = Maybe.map (S.ValidateToken >> SessionCmd) opt.authToken
            |> Maybe.map CE.pure
            |> Maybe.withDefault Cmd.none
    in (AppModel env model, cmd)

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
        , router = mkRouter navKey
        }

update : Command -> AppModel -> (AppModel, Cmd Command)
update cmd app = 
    case (cmd) of 
        (SessionCmd sessionCmd) ->
            let session = P.pageSession app.currentPage
                fxSession = S.updateFx app.env sessionCmd session
                fxInterpreter = 
                    Fx.runFxComp runLocalStorageFx (runHttpFx app.env.baseUrl)
                    |> Fx.runFxComp runCommandFx 
                (newSession, newCmd) = Fx.runFx fxInterpreter fxSession
                newPageModel = P.updateSession newSession app.currentPage
            in ({ app | currentPage = newPageModel }, Cmd.map SessionCmd newCmd)

        (RoutingCmd routingCmd) -> 
            let (x, newCmd) = R.handleRouting app.env routingCmd app.currentPage
            in ( { app | currentPage = x }, Cmd.map PageCmd newCmd)

        (PageCmd pageCmd) ->
            let fxPage = 
                    P.update app.env pageCmd app.currentPage
                    |> Fx.mapFx (Fx.mapLeft (FxH.map PageCmd))
                    |> Fx.mapFx (Fx.mapRight (FxAT.map (S.ValidateToken >> SessionCmd)))
                fxInterpreter =
                    Fx.runFxComp (runHttpFx app.env.baseUrl) runAuthenticationTokenFx
                (newPageModel, newCmd) = Fx.runFx fxInterpreter fxPage
            in ({ app | currentPage = newPageModel }, newCmd)

        (_) -> (app, Cmd.none)

type alias AppModel = 
    { env : Environment
    , currentPage : P.PageModel
    }

type Command 
    = PageCmd P.PageCommand
    | SessionCmd S.Command
    | RoutingCmd R.Command
    | Noop

subscriptions : AppModel -> Sub Command
subscriptions model = 
    LS.listenStringStorageKeyChange model.env.sessionSettings.cacheKey
    |> Sub.map (Maybe.map S.ValidateToken >> Maybe.withDefault S.Logout)
    |> Sub.map SessionCmd

view : AppModel -> Browser.Document Command
view app = P.view app.env  app.currentPage
    |> documentMap PageCmd
    
documentMap : (a -> b) -> Browser.Document a -> Browser.Document b
documentMap f { title, body } = 
    { title = title
    , body = List.map (Html.map f) body
    }

-- loggedView : HE.HasBaseUrl (Lang.HasDict a) -> AuthorizedModel -> Html Command
-- loggedView d model = 
--     let
--         header = div 
--             [ class "header" ]
--             [ img [ src Assets.logo ] [] 
--             , div 
--                 [ class "logged-user" ]
--                 [ span 
--                     [ class "username" ]
--                     [ text ((Auth.user >> Auth.userName) model.authSession) ]
--                 , span 
--                     [ class "logout-btn" 
--                     , onClick (Session S.Logout) ] 
--                     [ text d.dict.logoutAction ] 
--                 ]
--             ]
--         pageView = 
--             case model.model of
--                 ScoreboardPage m -> SB.view d m
--         content = div [ class "content" ] [ pageView ]
--     in
--         div [ class "logged-container" ]
--             [ header
--             , content 
--             ]
