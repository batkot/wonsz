module IO.Api exposing
    ( login
    , renewToken

    , getSeasonOverview
    , SeasonOverview
    , SeasonParticipant

    , getAccountDetails
    , AccountDetails

    , ChangePasswordRequest
    , changePassword

    , ScoreboardSummary
    , getScoreboardSummary
    )

import Http exposing (jsonBody, emptyBody)
import Http.Extra exposing (makeRequest, HttpRequest, HttpMethod(..), Url(..))
import Auth exposing (authorize, Requires, AuthSession)

import Json.Encode as JE
import Json.Decode as JD
import Json.Decode.Pipeline as JDP

import Url.Builder as UB

type alias AuthTokenString = String
type alias Username = String
type alias Password = String

type alias Authorized res = Requires AuthSession (HttpRequest res)

login : Username -> Password -> HttpRequest AuthTokenString
login username password =
    let loginBody = JE.object
            [ ("username", JE.string username)
            , ("password", JE.string password)]
            |> jsonBody
    in makeRequest (Url "/auth/login") Post JD.string loginBody

renewToken : Authorized AuthTokenString
renewToken =
    makeRequest (Url "/auth/renewToken") Post JD.string emptyBody
    |> authorize

type alias SeasonOverview =
    { participants : List SeasonParticipant
    }

type alias SeasonParticipant =
    { name : String
    , score : Int
    , place : Int
    , avatarUrl : String
    }

seasonOverviewDecoder : JD.Decoder SeasonOverview
seasonOverviewDecoder =
    JD.succeed SeasonOverview
    |> JDP.required "participants" (JD.list seasonParticipantDecoder)

seasonParticipantDecoder : JD.Decoder SeasonParticipant
seasonParticipantDecoder =
    JD.succeed SeasonParticipant
    |> JDP.required "participantName" JD.string
    |> JDP.required "participantScore" JD.int
    |> JDP.required "participantPlace" JD.int
    |> JDP.required "participantAvatarUrl" JD.string

getSeasonOverview : Authorized SeasonOverview
getSeasonOverview =
    makeRequest (Url "/api/season/overview") Get seasonOverviewDecoder emptyBody
    |> authorize

type alias AccountDetails =
    { id : Int
    , login : String
    , name : String
    , avatarUrl : String
    }

accountDetailsDecoder : JD.Decoder AccountDetails
accountDetailsDecoder =
    JD.succeed AccountDetails
    |> JDP.required "id" JD.int
    |> JDP.required "login" JD.string
    |> JDP.required "name" JD.string
    |> JDP.required "avatarUrl" JD.string

getAccountDetails : Int -> Authorized AccountDetails
getAccountDetails accountId =
    let url = "/api/account/" ++ String.fromInt accountId |> Url
    in makeRequest url Get accountDetailsDecoder emptyBody
        |> authorize

type alias ChangePasswordRequest =
    { newPassword : String
    , currentPassword : String
    }

changePassword : ChangePasswordRequest -> Authorized {}
changePassword request =
    let requestBody = JE.object
            [ ("newPassword", JE.string request.newPassword)
            , ("currentPassword", JE.string request.currentPassword)]
            |> jsonBody
    in makeRequest (Url "/api/account/changePassword") Post (JD.succeed {}) requestBody
        |> authorize

type alias ScoreboardSummary =
    { id : Int
    , name : String
    , participantsCount : Int
    , leader : AccountDetails
    }

scoreboardSummaryDecoder : JD.Decoder ScoreboardSummary
scoreboardSummaryDecoder = 
    JD.succeed ScoreboardSummary
    |> JDP.required "id" JD.int
    |> JDP.required "name" JD.string
    |> JDP.required "participantsCount" JD.int
    |> JDP.required "leader" accountDetailsDecoder

type alias AccountId = Int

getScoreboardSummary : AccountId -> Authorized (List ScoreboardSummary)
getScoreboardSummary accountId = 
    let url = UB.absolute [ "api", "dashboard", String.fromInt accountId ] [] |> Url
    in makeRequest url Get (JD.list scoreboardSummaryDecoder) emptyBody
        |> authorize
