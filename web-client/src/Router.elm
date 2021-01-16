module Router exposing
    ( Command(..)
    , HasNavKey

    , handleRouting
    )

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key)
import Url

type alias HasNavKey a = { a | navKey : Key }

type Command = RouteRequested UrlRequest

-- High level api
handleRouting : HasNavKey a -> Command -> Cmd cmd
handleRouting { navKey } (RouteRequested request) =
    case request of
        Internal url ->
            Url.toString url
            |> Nav.pushUrl navKey

        External url ->
            Nav.load url


