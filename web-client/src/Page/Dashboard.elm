module Page.Dashboard exposing 
    ( init
    , Model

    , Command
    , update

    , view
    )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

import Http.Extra as HE exposing (HasBaseUrl)
import Html.Extra exposing (spinner, exclamationMessage)
import Lang exposing (HasDict)
import Auth exposing (UserDescription, AuthSession)

import Effect as Fx exposing (Fx)
import Effect.Http exposing (HttpFx(..))
import Effect.Command exposing (CommandFx(..))

import IO.Api exposing (getScoreboardSummary, ScoreboardSummary)

import Scoreboard exposing (view)

type alias DashboardData = List ScoreboardSummary

type Command 
    = LoadDashboard Int
    | GotScoreboardSummary (List ScoreboardSummary)
    | GotError 

type Model 
    = LoadingDashboard Int
    | DashboardLoaded DashboardData
    | Error 

init : UserDescription -> Fx (CommandFx Command) Model
init user = 
    let userId = Auth.userId user
    in LoadingDashboard userId
        |> Fx.addFx (Raise (LoadDashboard userId))

update : AuthSession -> Command -> Model -> Fx (HttpFx Command) Model
update auth cmd _ =
    case cmd of 
        LoadDashboard accountId -> 
            let apiCall = getScoreboardSummary accountId auth
                            |> HE.mapRequest GotScoreboardSummary
                httpFx = Request apiCall (always GotError)
            in LoadingDashboard accountId 
                |> Fx.addFx httpFx

        GotScoreboardSummary data -> 
            DashboardLoaded data |> Fx.pure
        GotError -> Fx.pure Error

view : HasBaseUrl (HasDict a) -> Model -> Html cmd
view env model = 
    div [ class "dashboard" ] 
        [ div 
            [ class "title" ]
            [ text <| env.dict.dashboardTitle ]
        , foo env model
        ]

foo : HasDict a -> Model -> Html cmd
foo { dict } model =
    case model of 
        LoadingDashboard _ -> spinner
        DashboardLoaded x -> List.length x |> String.fromInt |> text
        Error -> exclamationMessage dict.loadErrorMessage
