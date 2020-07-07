module Session exposing 
    ( Session(..)
    , SessionSettings
    , HasSessionSettings
    , Command(..)
    , update
    )

import Auth exposing (AuthSession, TokenString)

import Result.Extra as RE

import Http.Extra as HE exposing (HasBaseUrl)
import IO.Api as Api
import IO.LocalStorage as LS
import Delay as D

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
