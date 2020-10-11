module Page exposing
    ( PageModel
    , PageCommand
    , requireLogin

    , update
    , updateSession
    , pageSession

    , view
    )

import Http.Extra as HE

import Browser exposing (Document)
import Html exposing (Html)

import Lang exposing (HasDict)

import Auth exposing (AuthSession)
import Session exposing (Session(..))

import Effect as Fx exposing (Fx)
import Effect.Http as HttpFx exposing (HttpFx(..)) 
import Effect.AuthenticationToken exposing (AuthenticationTokenFx)
import Effect.Compose exposing (FxComp, mapLeft)

import Login as L 

type alias Page pageModel = 
    { title : String 
    , model : pageModel }


type alias Authorized a = { a | authSession : AuthSession }
type AccountData = AccountData
type AccountCmd = AccountCmd

type alias PageFx = FxComp (HttpFx PageCommand) (AuthenticationTokenFx String)

type PageModel
    = Login (Page L.LoginData)
    | Account (Authorized (Page AccountData))
    | NotFound (Authorized (Page {}))

type PageCommand
    = LoginCommand L.LoginCmd
    | AccountCommand AccountCmd

update : HE.HasBaseUrl a -> PageCommand -> PageModel -> Fx PageFx PageModel
update baseUrl command model = 
    case (command, model) of
        (LoginCommand cmd, Login loginPage) -> 
            L.update cmd loginPage.model
            |> Fx.mapFx (mapLeft (HttpFx.map LoginCommand))
            |> Fx.map (\l -> Login { loginPage | model = l })

        (_, _) -> Fx.pure model

updateSession : Session -> PageModel -> PageModel
updateSession session page = 
    case (session, page) of 
        (Anonymous, Login x) 
            -> Login x
        (Anonymous, _) 
            -> requireLogin
        (Authenticated auth, Login l) 
            -> requireLogin

        -- Boring dispatch -.-
        (Authenticated auth, Account p) -> Account <| updateAuthSession auth p
        (Authenticated auth, NotFound p) -> NotFound <| updateAuthSession auth p

pageSession : PageModel -> Session
pageSession page = 
    case page of 
        (Login _) -> Anonymous
        (Account p) -> getSession p
        (NotFound p) -> getSession p

requireLogin : PageModel
requireLogin = Login 
    { title = "Wonsz - login" 
    , model = L.emptyModel Nothing }

updateAuthSession : AuthSession -> Authorized a -> Authorized a
updateAuthSession session authorized = { authorized | authSession = session }

getSession : Authorized a -> Session
getSession { authSession } = Authenticated authSession

view : HasDict a -> PageModel -> Document PageCommand
view hasDict model = 
    case model of 
        (Login loginPage) -> 
            L.view hasDict loginPage.model
            |> Html.map LoginCommand
            |> makeDocument loginPage.title

        (Account accountPage) ->
            Html.div [] []
            |> makeDocument accountPage.title

        (NotFound notFoundPage) ->
            Html.div [] []
            |> makeDocument notFoundPage.title

makeDocument : String -> Html a -> Document a
makeDocument title html = 
    { title = title
    , body = [ html ]
    }
