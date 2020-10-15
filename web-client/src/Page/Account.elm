module Page.Account exposing
    ( init
    , Command
    , Model

    , update
    )

import Http.Extra as HE

import Auth exposing (AuthSession)

import Effect as Fx exposing (Fx)
import Effect.Compose as Fx exposing (FxComp)
import Effect.Http exposing (HttpFx(..))
import Effect.Command exposing (CommandFx(..))

import IO.Api exposing (getAccountDetails, AccountDetails)

type Command
    = ShowAccount AccountId 
    | GotAccount AccountDetails
    | GotError String

type Model
    = Loading AccountId
    | Loaded AccountDetails
    | Error String

update : AuthSession -> Command -> Model -> Fx (HttpFx Command) Model
update auth command model =
    case command of
        ShowAccount accountId -> 
            let apiCall = getAccountDetails accountId auth
                        |> HE.mapRequest GotAccount
                httpFx = Request apiCall (always (GotError ":("))
            in Loading accountId
                |> Fx.addFx httpFx

        _ -> Fx.pure model

type alias AccountId = Int

init : AccountId -> Fx (CommandFx Command) Model
init accountId = 
    Loading accountId
    |> Fx.addFx (Raise (ShowAccount accountId))
