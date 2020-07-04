module IO.Api exposing 
    ( login

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
            [ ("user", JE.string username)
            , ("password", JE.string password)]
            |> jsonBody
    in makeRequest (Url "/login") Post JD.string loginBody

type alias OverviewDto =
    { something : String
    }

overviewDtoDecoder : JD.Decoder OverviewDto
overviewDtoDecoder = 
    JD.succeed OverviewDto
    |> JDP.required "something" JD.string

overview : Authorized OverviewDto
overview = 
    makeRequest (Url "/overview") Get overviewDtoDecoder emptyBody
    |> authorize
