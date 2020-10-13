module Page.Account exposing
    ( init
    , Command
    , Model
    )

import Auth exposing (AuthSession)

import Effect as Fx exposing (Fx)
import Effect.Compose as Fx exposing (FxComp)
import Effect.Http exposing (HttpFx)
import Effect.Command exposing (CommandFx(..))

type Command
    = ShowAccount AccountId 

type Model
    = Loading AccountId
    | Loaded AccountDetails
    | Error String

type alias AccountDetails =
    { id : AccountId
    , login : String
    , name : String
    }

type alias AccountId = Int

init : AccountId -> Fx (CommandFx Command) Model
init accountId = 
    Loading accountId
    |> Fx.addFx (Raise (ShowAccount accountId))
