module AuthSpec exposing (tests)

import Auth exposing (TokenString, parseToken, user, userName, createAuthHeader)

import Http
import Json.Encode as JE
import Base64

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test, fuzz, describe)

tests : Test
tests = describe "Auth specs"
    [ authTokenParsingTests
    , headerGenerationTests
    ]

authTokenParsingTests : Test
authTokenParsingTests = describe "Token parsing"
    [ fuzz validTokenContextFuzz "Valid token should create AuthSession" parsingValidTokenShouldReturnAuthSession
    , fuzz invalidTokenFuzz "Invalid token should return Nothing" parsingInvalidTokenShouldReturnNothing
    ]

type ValidTokenString = ValidTokenString TokenString
type InvalidTokenString = InvalidTokenString TokenString

type alias ValidTokenContext =
    { expectedUserName : String
    , token : ValidTokenString
    }

validTokenContextFuzz : Fuzzer ValidTokenContext
validTokenContextFuzz =
    let createContext id name = ValidTokenContext name (createValidToken id name)
    in Fuzz.map2 createContext Fuzz.int Fuzz.string

createValidToken : Int -> String -> ValidTokenString
createValidToken userId userName =
    let usrObj = JE.object
            [ ("auId", JE.int userId)
            , ("name", JE.string userName)
            ]
        header = "eyJhbGciOiJIUzUxMiJ9"
        sig = "ojwwz5qnB6Lxzi56a1UeGCmz0MgriR1z0gSTQEIlfSIP1GAJdhuJzzHjoYAoflq_cR5dhQp4afJ9xB4t3HCMmA"
    in JE.object [ ("dat", usrObj) ]
    |> JE.encode 0
    |> Base64.encode
    |> (++) (header ++ ".")
    |> \x -> x ++ "." ++ sig
    |> ValidTokenString

validTokenFuzz : Fuzzer ValidTokenString
validTokenFuzz = Fuzz.map2 createValidToken Fuzz.int Fuzz.string

invalidTokenFuzz : Fuzzer InvalidTokenString
invalidTokenFuzz = Fuzz.string
    |> Fuzz.map InvalidTokenString

parsingValidTokenShouldReturnAuthSession : ValidTokenContext -> Expectation
parsingValidTokenShouldReturnAuthSession testContext =
    let (ValidTokenString token) = testContext.token
    in parseToken token
    |> Maybe.map (user >> userName)
    |> Expect.equal (Just testContext.expectedUserName)

parsingInvalidTokenShouldReturnNothing : InvalidTokenString -> Expectation
parsingInvalidTokenShouldReturnNothing (InvalidTokenString token) =
    parseToken token
    |> Expect.equal Nothing

headerGenerationTests : Test
headerGenerationTests = describe "Auth header generation"
    [ fuzz validTokenFuzz "Creates Authorization Bearer token header" createHeaderShouldPutAuthTokenInAuthorizationHeader
    ]

createHeaderShouldPutAuthTokenInAuthorizationHeader : ValidTokenString -> Expectation
createHeaderShouldPutAuthTokenInAuthorizationHeader (ValidTokenString token) =
    let expectedHeader = Http.header "Authorization" ("Bearer " ++ token)
    in parseToken token
    |> Maybe.map createAuthHeader
    |> Expect.equal (Just expectedHeader)
