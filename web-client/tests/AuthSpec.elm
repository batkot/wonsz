module AuthSpec exposing (tests)

import Auth exposing (TokenString, authorize, parseToken, user, userName)
import Base64
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Http
import Http.Extra as HE
import Json.Decode as JD
import Json.Encode as JE
import Test exposing (Test, describe, fuzz, fuzz2)


tests : Test
tests =
    describe "Auth specs"
        [ authTokenParsingTests
        , authorizeTests
        ]


authTokenParsingTests : Test
authTokenParsingTests =
    describe "Token parsing"
        [ fuzz validTokenContextFuzz "Valid token should create AuthSession" parsingValidTokenShouldReturnAuthSession
        , fuzz invalidTokenFuzz "Invalid token should return Nothing" parsingInvalidTokenShouldReturnNothing
        ]


type ValidTokenString
    = ValidTokenString TokenString


type InvalidTokenString
    = InvalidTokenString TokenString


type alias ValidTokenContext =
    { expectedUserName : String
    , token : ValidTokenString
    }


validTokenContextFuzz : Fuzzer ValidTokenContext
validTokenContextFuzz =
    let
        createContext id name =
            ValidTokenContext name (createValidToken id name)
    in
    Fuzz.map2 createContext Fuzz.int Fuzz.string


createValidToken : Int -> String -> ValidTokenString
createValidToken userId userName =
    let
        usrObj =
            JE.object
                [ ( "auId", JE.int userId )
                , ( "name", JE.string userName )
                ]

        header =
            "eyJhbGciOiJIUzUxMiJ9"

        sig =
            "ojwwz5qnB6Lxzi56a1UeGCmz0MgriR1z0gSTQEIlfSIP1GAJdhuJzzHjoYAoflq_cR5dhQp4afJ9xB4t3HCMmA"
    in
    JE.object [ ( "dat", usrObj ) ]
        |> JE.encode 0
        |> Base64.encode
        |> (++) (header ++ ".")
        |> (\x ->
                x
                    ++ "."
                    ++ sig
                    |> ValidTokenString
           )


validTokenFuzz : Fuzzer ValidTokenString
validTokenFuzz =
    Fuzz.map2 createValidToken Fuzz.int Fuzz.string


invalidTokenFuzz : Fuzzer InvalidTokenString
invalidTokenFuzz =
    Fuzz.string
        |> Fuzz.map InvalidTokenString


parsingValidTokenShouldReturnAuthSession : ValidTokenContext -> Expectation
parsingValidTokenShouldReturnAuthSession testContext =
    let
        (ValidTokenString token) =
            testContext.token
    in
    parseToken token
        |> Maybe.map (user >> userName)
        |> Expect.equal (Just testContext.expectedUserName)


parsingInvalidTokenShouldReturnNothing : InvalidTokenString -> Expectation
parsingInvalidTokenShouldReturnNothing (InvalidTokenString token) =
    parseToken token
        |> Expect.equal Nothing


authorizeTests : Test
authorizeTests =
    describe "Authorize tests"
        [ fuzz2 (httpRequestFuzz Fuzz.int) validTokenFuzz "Creates Authorization Bearer token header" authorizeHttpRequestShouldAddAuthenticationHeader
        ]


httpMethodFuzz : Fuzzer HE.HttpMethod
httpMethodFuzz =
    [ HE.Get, HE.Post, HE.Put, HE.Delete ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


httpRequestFuzz : Fuzzer a -> Fuzzer (HE.HttpRequest a)
httpRequestFuzz =
    Fuzz.map3 (\url method x -> HE.makeRequest (HE.Url url) method (JD.succeed x) Http.emptyBody) Fuzz.string httpMethodFuzz


authorizeHttpRequestShouldAddAuthenticationHeader : HE.HttpRequest a -> ValidTokenString -> Expectation
authorizeHttpRequestShouldAddAuthenticationHeader request (ValidTokenString token) =
    let
        expectedHeader =
            Http.header "Authorization" ("Bearer " ++ token)

        expectedRequest =
            HE.addHeader expectedHeader request
    in
    parseToken token
        |> Maybe.map (authorize request)
        |> Expect.equal (Just expectedRequest)
