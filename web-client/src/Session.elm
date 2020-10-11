module Session exposing 
    ( Session(..)
    , SessionSettings
    , HasSessionSettings
    , Command(..)
    , update

    , updateFx
    )

import Auth exposing (AuthSession, TokenString)

import Result.Extra as RE

import Http.Extra as HE exposing (HasBaseUrl)
import IO.Api as Api
import IO.LocalStorage as LS
import Delay as D

import Effect as Fx exposing (Fx)
import Effect.Command exposing (CommandFx(..))
import Effect.Http exposing (HttpFx(..))
import Effect.LocalStorage exposing (LocalStorageFx(..))
import Effect.Compose as Fx exposing (FxComp, next)

type Session 
    = Anonymous
    | Authenticated AuthSession

type Command 
    = ValidateToken TokenString
    | RenewToken 
    | Logout  

type alias SessionSettings =
    { cacheKey : String
    -- Base it later on token expiration
    -- Token refresh isn't a good idea in general
    , sessionRefresh : Float
    }

type alias HasSessionSettings a = { a | sessionSettings : SessionSettings } 

update : HasSessionSettings (HasBaseUrl a) -> Command -> Session -> (Session, Cmd Command)
update { baseUrl, sessionSettings } command session =
    case (command, session) of
        (ValidateToken token, _) -> 
            authenticateToken sessionSettings token

        (_, Anonymous) -> (session, Cmd.none)

        (RenewToken, (Authenticated auth)) -> 
            let renewRequest = Api.renewToken auth
                    |> HE.execute baseUrl
                    |> Cmd.map (RE.unpack (always Logout) ValidateToken)
            in (session, renewRequest)

        (Logout, (Authenticated _)) -> (Anonymous, LS.clearKey sessionSettings.cacheKey)

authenticateToken : SessionSettings -> TokenString -> (Session, Cmd Command)
authenticateToken settings token = 
    let fx = Cmd.batch
            [ LS.storeString settings.cacheKey token
            , D.after settings.sessionRefresh D.Minute RenewToken
            ]
    in Auth.parseToken token
        |> Maybe.map Authenticated
        |> Maybe.map (\newSession -> (newSession, fx))
        |> Maybe.withDefault (Anonymous, Cmd.none)

type alias SessionFx = FxComp (CommandFx Command) (FxComp LocalStorageFx (HttpFx Command))

updateFx 
    : HasSessionSettings a 
    -> Command 
    -> Session 
    -> Fx SessionFx Session
updateFx { sessionSettings } command session =
    case (command, session) of
        (ValidateToken token, _) -> 
            let cmdFx = Delay RenewToken sessionSettings.sessionRefresh
                lsFx = Store sessionSettings.cacheKey token
            in Auth.parseToken token
                |> Maybe.map Authenticated
                |> Maybe.withDefault Anonymous
                |> Fx.pure
                |> Fx.pushLeft lsFx
                |> Fx.pushLeft cmdFx

        (_, Anonymous) -> Fx.pure session

        (RenewToken, Authenticated auth) -> 
            let apiRequest = Api.renewToken auth |> HE.mapRequest ValidateToken
                httpFx = Request apiRequest (always Logout)
            in Fx.pure session
                |> Fx.push ((next >> next) httpFx)

        (Logout, Authenticated _) -> 
            Fx.pure Anonymous
            |> Fx.pushLeft (Clear sessionSettings.cacheKey)
            |> Fx.mapFx next
