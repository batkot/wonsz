module IO.Api exposing 
    ( login
    , renewToken

    , overview
    , OverviewDto
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

type alias OverviewDto =
    { something : String
    }

overviewDtoDecoder : JD.Decoder OverviewDto
overviewDtoDecoder = 
    JD.succeed OverviewDto
    |> JDP.required "something" JD.string

overview : Authorized OverviewDto
overview = 
    makeRequest (Url "/api/overview") Get overviewDtoDecoder emptyBody
    |> authorize

renewToken : Authorized AuthTokenString
renewToken = 
    makeRequest (Url "/auth/renewToken") Post JD.string emptyBody
    |> authorize
