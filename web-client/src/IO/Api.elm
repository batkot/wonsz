module IO.Api exposing 
    ( login
    , renewToken

    , getSeasonOverview
    , SeasonOverview
    , SeasonParticipant
    )

import Http exposing (jsonBody, emptyBody)
import Http.Extra exposing (makeRequest, HttpRequest, HttpMethod(..), Url(..))
import Auth exposing (authorize, Requires, AuthSession)

import Json.Encode as JE
import Json.Decode as JD
import Json.Decode.Pipeline as JDP

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
    makeRequest (Url "/api/overview") Get seasonOverviewDecoder emptyBody
    |> authorize

renewToken : Authorized AuthTokenString
renewToken = 
    makeRequest (Url "/auth/renewToken") Post JD.string emptyBody
    |> authorize
