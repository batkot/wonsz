module Page.Dashboard exposing 
    ( init
    , Model

    , Command
    , update

    , view
    )

import Html exposing (Html, div, text, a, img)
import Html.Attributes exposing (class, href, src)

import Http.Extra as HE exposing (HasBaseUrl, Url(..))
import Html.Extra exposing (spinner, exclamationMessage)
import Lang exposing (HasDict)
import Auth exposing (UserDescription, AuthSession)

import Effect as Fx exposing (Fx)
import Effect.Http exposing (HttpFx(..))
import Effect.Command exposing (CommandFx(..))

import IO.Api exposing (getScoreboardSummary, ScoreboardSummary)
import Router.Routes as RR

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
    let
        dashboard = case model of 
            LoadingDashboard _ -> spinner
            DashboardLoaded boards -> dashboardView env boards
            Error -> exclamationMessage env.dict.loadErrorMessage
    in 
        div [ class "dashboard" ] 
            [ dashboard ]

dashboardView : HasBaseUrl a -> DashboardData -> Html cmd
dashboardView { baseUrl } scoreboards = 
    List.map (scoreboardOverview baseUrl) scoreboards
    |> div [ class "scoreboards"]

scoreboardOverview : Url -> ScoreboardSummary -> Html cmd
scoreboardOverview (Url baseUrl) scoreboard = 
    a 
        [ class "scoreboard" 
        , href (RR.toUrl (RR.Scoreboard scoreboard.id)) 
        ] 
        [ div 
            [ class "name" ]
            [ text scoreboard.name
            ]
        , div 
            [ class "leader-avatar" ]
            [ img [src (baseUrl ++ scoreboard.leader.avatarUrl)] []
            ]
        ]
