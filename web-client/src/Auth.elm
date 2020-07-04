module Auth exposing
    ( AuthSession
    , createAuthHeader
    , user

    , UserDescription
    , userName

    , TokenString
    , parseToken
    )

import Http
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
    let
        decoder = JD.field "dat" userDescriptionDecoder
    in
        Jwt.decodeToken decoder tokenString
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

createAuthHeader : AuthSession -> Http.Header
createAuthHeader (AuthSession session) = "Bearer " ++ unwrapToken session.authToken
    |> Http.header "Authorization" 

type UserDescription = UserDescription UserData

type alias UserData =
    { id : Int
    , userName : String
    }

userName : UserDescription -> String
userName (UserDescription u) = u.userName

userDescriptionDecoder : JD.Decoder UserDescription
userDescriptionDecoder = 
    JD.succeed UserData
    |> JDP.required "auId" JD.int
    |> JDP.required "name" JD.string
    |> JD.map UserDescription
