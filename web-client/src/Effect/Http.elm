module Effect.Http exposing
    ( HttpFx(..)

    , runHttpFx
    , map
    )

import Http
import Http.Extra as HE
import Result.Extra as RE

type HttpFx msg
    = Request (HE.HttpRequest msg) (Http.Error -> msg)

runHttpFx : HE.Url -> HttpFx msg -> Cmd msg
runHttpFx baseUrl (Request request f) = 
    HE.execute baseUrl request 
    |> Cmd.map (RE.unpack f identity)

map : (a -> b) -> HttpFx a -> HttpFx b
map f (Request request oldF) = Request (HE.mapRequest f request) (oldF >> f)
