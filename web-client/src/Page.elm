module Page exposing
    ( PageModel
    , PageCommand
    , requireLogin

    , update
    , updateSession
    , pageSession

    , view
    , PageView
    , pageViewMap

    , requestPage
    )

import Html exposing (Html)

import Http.Extra exposing (HasBaseUrl)

import Lang exposing (HasDict)

import Auth exposing (AuthSession)
import Session exposing (Session(..))

import Effect as Fx exposing (Fx)
import Effect.Http as HttpFx exposing (HttpFx(..))
import Effect.Command as CommandFx exposing (CommandFx(..))
import Effect.AuthenticationToken exposing (AuthenticationTokenFx)
import Effect.Compose exposing (FxComp, mapLeft, here, next)

import Router.Routes exposing (Route(..))

import Login as L
import Page.Account as A
import Page.Dashboard as D
import Page.NotFound as NF

type alias Page pageModel =
    { title : String
    , model : pageModel
    , route : Route }

type alias Authorized a = { a | authSession : AuthSession }

type alias PageView cmd =
    { title : String
    , html : Html cmd
    }

pageViewMap : (a -> b) -> PageView a -> PageView b
pageViewMap f { title, html } =
    { title = title
    , html = Html.map f html
    }

type alias PageFx =
    FxComp (CommandFx PageCommand)
    (FxComp (HttpFx PageCommand) (AuthenticationTokenFx String))

type PageModel
    = Login (Page L.LoginData)
    | Account (Authorized (Page A.Model))
    | Dashboard (Authorized (Page D.Model))
    | NotFound (Authorized (Page {}))

type PageCommand
    = LoginCommand L.LoginCmd
    | AccountCommand A.Command
    | DashboardCommand D.Command
    | ChangePage Route

requestPage : Route -> PageCommand
requestPage = ChangePage

update : HasDict a -> PageCommand -> PageModel -> Fx PageFx PageModel
update env command model =
    case (command, model) of
        (LoginCommand cmd, Login loginPage) ->
            L.update cmd loginPage.model
            |> Fx.mapFx (mapLeft (HttpFx.map LoginCommand))
            |> Fx.mapFx next
            |> Fx.map (\l -> Login { loginPage | model = l })

        (ChangePage newRoute, page) ->
            if toRoute page == newRoute
            then Fx.pure model
            else dispatchRoute env (pageSession page) newRoute
                    |> Fx.mapFx here

        (AccountCommand cmd, Account accountPage) ->
                A.update accountPage.authSession cmd accountPage.model
                |> Fx.mapFx (HttpFx.map AccountCommand)
                |> Fx.mapFx (here >> next)
                |> Fx.map (\a -> Account { accountPage | model = a})

        (DashboardCommand cmd, Dashboard dashboardPage) ->
                D.update dashboardPage.authSession cmd dashboardPage.model
                |> Fx.mapFx (HttpFx.map DashboardCommand)
                |> Fx.mapFx (here >> next)
                |> Fx.map (\d -> Dashboard { dashboardPage | model = d})

        (_, _) -> Fx.pure model

updateSession : HasDict a -> Session -> PageModel -> Fx (CommandFx PageCommand) PageModel
updateSession env session page =
    case (session, page) of
        (Anonymous, Login x)
            -> Fx.pure <| Login x
        (Anonymous, p)
            -> toRoute p
                |> requireLogin env.dict.loginPageTitle
                |> Fx.pure
        (Authenticated _, Login l)
            -> dispatchRoute env session l.route

        -- Boring dispatch -.-
        (Authenticated auth, Account p) -> Fx.pure <| Account <| updateAuthSession auth p
        (Authenticated auth, NotFound p) -> Fx.pure <| NotFound <| updateAuthSession auth p
        (Authenticated auth, Dashboard p) -> Fx.pure <| Dashboard <| updateAuthSession auth p

dispatchRoute : HasDict a -> Session -> Route -> Fx (CommandFx PageCommand) PageModel
dispatchRoute { dict } session route =
    case (session, route) of
        (Anonymous, r)
            -> requireLogin dict.loginPageTitle r
                |> Fx.pure

        (Authenticated auth, Router.Routes.NotFound)
            -> Fx.pure <| NotFound <|
                { authSession = auth
                , title = createTitle dict.notFoundPageTitle
                , route = route
                , model = {}
                }

        (Authenticated auth, Router.Routes.Account accountId)
            -> A.init accountId
                |> Fx.map (\model -> Account
                    { authSession = auth
                    , title = createTitle dict.accountPageTitle
                    , model = model
                    , route = route
                    })
                |> Fx.mapFx (CommandFx.map AccountCommand)

        (Authenticated auth, Router.Routes.Dashboard)
            -> D.init (Auth.user auth)
                |> Fx.map (\model -> Dashboard
                    { authSession = auth
                    , title = createTitle dict.dashboardPageTitle
                    , route = route
                    , model = model
                    })
                |> Fx.mapFx (CommandFx.map DashboardCommand)

        (Authenticated auth, Router.Routes.Scoreboard _)
            -> Fx.pure <| NotFound <|
                { authSession = auth
                , title = createTitle dict.scoreboardPageTitle
                , route = route
                , model = {}
                }

pageSession : PageModel -> Session
pageSession page =
    case page of
        (Login _) -> Anonymous
        (Account p) -> getSession p
        (NotFound p) -> getSession p
        (Dashboard p) -> getSession p

toRoute : PageModel -> Route
toRoute model =
    case model of
        Login p -> p.route
        Account p -> p.route
        NotFound p -> p.route
        Dashboard p -> p.route

requireLogin : String-> Route -> PageModel
requireLogin  loginTitle redirectTo = Login
    { title = createTitle loginTitle
    , route = redirectTo
    , model = L.emptyModel redirectTo }

createTitle : String -> String
createTitle subtitle = "Wonsz - " ++ subtitle

updateAuthSession : AuthSession -> Authorized a -> Authorized a
updateAuthSession session authorized = { authorized | authSession = session }

getSession : Authorized a -> Session
getSession { authSession } = Authenticated authSession

view : HasBaseUrl (HasDict a) -> PageModel -> PageView PageCommand
view env model =
    case model of
        (Login loginPage) ->
            L.view env loginPage.model
            |> Html.map LoginCommand
            |> pageView loginPage.title

        (Account accountPage) ->
            A.view env accountPage.model
            |> Html.map AccountCommand
            |> pageView accountPage.title

        (Dashboard dashboardPage) ->
            D.view env dashboardPage.model
            |> pageView dashboardPage.title

        (NotFound notFoundPage) ->
            NF.view env
            |> pageView notFoundPage.title

pageView : String -> Html a -> PageView a
pageView title html =
    { title = title
    , html = html
    }
