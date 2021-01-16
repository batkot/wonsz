module Auth exposing
    ( AuthSession
    , user

    , UserDescription
    , userName
    , userId

    , TokenString
    , parseToken

    , Requires
    , authorize

    -- Token Management
    , authenticate
    , logout
    )

import Http
import Http.Extra as HE

import IO.LocalStorage as LS

import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Jwt
import Result

type AuthToken = AuthToken String

unwrapToken : AuthToken -> String
unwrapToken (AuthToken t) = t

type alias TokenString = String

parseToken : TokenString -> Maybe AuthSession
parseToken tokenString =
    let decoder = JD.field "dat" userDescriptionDecoder
    in Jwt.decodeToken decoder tokenString
        |> Result.map (\usr -> AuthSession
            { authToken = AuthToken tokenString
            , userData =  usr
            })
        |> Result.toMaybe

type AuthSession = AuthSession
    { authToken : AuthToken
    , userData : UserDescription
    }

user : AuthSession -> UserDescription
user (AuthSession session) = session.userData

type UserDescription = UserDescription UserData

type alias UserData =
    { id : Int
    , userName : String
    }

userName : UserDescription -> String
userName (UserDescription u) = u.userName

userId : UserDescription -> Int
userId (UserDescription u) = u.id

userDescriptionDecoder : JD.Decoder UserDescription
userDescriptionDecoder =
    JD.succeed UserData
    |> JDP.required "auId" JD.int
    |> JDP.required "name" JD.string
    |> JD.map UserDescription

authTokenStorageKey : String
authTokenStorageKey = "AuthToken"

-- Token lifecycle management
authenticate : TokenString -> Maybe (AuthSession, Cmd a)
authenticate token =
    parseToken token
    |> Maybe.map (\session -> (session, LS.storeString authTokenStorageKey token))

logout : Requires AuthSession (Cmd a)
logout = always <| LS.clearKey authTokenStorageKey

-- Http Authorize
type alias Requires auth res = auth -> res

authorize : HE.HttpRequest a -> Requires AuthSession (HE.HttpRequest a)
authorize req auth =
    let authHeader = createAuthHeader auth
    in HE.addHeader authHeader req

createAuthHeader : AuthSession -> Http.Header
createAuthHeader (AuthSession session) = "Bearer " ++ unwrapToken session.authToken
    |> Http.header "Authorization"
