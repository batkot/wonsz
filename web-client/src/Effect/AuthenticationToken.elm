module Effect.AuthenticationToken exposing
    ( AuthenticationTokenFx
    , raiseTokenAquired
    , runAuthenticationTokenFx

    , map
    )

import Effect.Command exposing (runCommandFx, CommandFx(..))

type AuthenticationTokenFx msg = TokenAquired msg

raiseTokenAquired : String -> AuthenticationTokenFx String
raiseTokenAquired token = TokenAquired token

runAuthenticationTokenFx : AuthenticationTokenFx msg -> Cmd msg
runAuthenticationTokenFx (TokenAquired msg) = Raise msg |> runCommandFx 

map : (a -> b) -> AuthenticationTokenFx a -> AuthenticationTokenFx b
map f (TokenAquired x) = TokenAquired <| f x
