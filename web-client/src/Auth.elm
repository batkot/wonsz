module Auth exposing
    ( Session
    , newSession
    , authUser

    , AuthSession
    , createAuthHeader

    , UserDescription
    , userName

    , parseToken
    )

-- Auth responsibilities:
-- Handle logged user
-- Store Auth Token
-- Parse JWT Token

import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP

type Session 
    = Anonymous
    | Authenticated AuthSession

newSession : Session
newSession = Anonymous

type AuthToken = AuthToken String

unwrapToken : AuthToken -> String
unwrapToken (AuthToken t) = t

type alias TokenString = String

parseToken : TokenString -> Maybe AuthSession
parseToken tokenString = Nothing

type AuthSession = AuthSession
    { authToken : AuthToken
    , userData : UserDescription
    }

createAuthHeader : AuthSession -> Http.Header
createAuthHeader (AuthSession session) = "Bearer " ++ unwrapToken session.authToken
    |> Http.header "Authorization" 

authUser : Session -> Maybe UserDescription
authUser session = case session of
    Anonymous -> Nothing
    Authenticated (AuthSession authSession) -> Just authSession.userData
        
type UserDescription = UserDescription
    { id : Int
    , userName : String
    }

userName : UserDescription -> String
userName (UserDescription user) = user.userName

userDescriptionDecoder : JD.Decoder UserDescription
userDescriptionDecoder = 
    JD.succeed UserDescription
    |> JDP.required "id" JD.int
    |> JDP.required "username" JD.string

