module Session exposing
    ( Session(..)
    , SessionSettings
    , HasSessionSettings
    , Command(..)

    , updateFx
    )

import Auth exposing (AuthSession, TokenString)

import Http.Extra as HE 
import IO.Api as Api

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
    , sessionRefresh : Int
    }

type alias HasSessionSettings a = { a | sessionSettings : SessionSettings }

type alias SessionFx = FxComp (CommandFx Command) (FxComp LocalStorageFx (HttpFx Command))

updateFx
    : HasSessionSettings a
    -> Command
    -> Session
    -> Fx SessionFx Session
updateFx { sessionSettings } command session =
    case (command, session) of
        (ValidateToken token, _) ->
            let mkRefreshFx s = Auth.expiresAt s - (sessionSettings.sessionRefresh * 60 * 1000)
                                    |> max 1 
                                    |> Schedule RenewToken
                lsFx = Store sessionSettings.cacheKey token
            in Auth.parseToken token
                |> Maybe.map 
                    (\s -> Authenticated s 
                            |> Fx.pure 
                            |> Fx.pushLeft lsFx 
                            |> Fx.pushLeft (mkRefreshFx s))
                |> Maybe.withDefault (Fx.pure Anonymous)

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
