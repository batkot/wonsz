module Router exposing 
    ( HasRouter
    , Router
    , Command(..)

    , handleRouting
    )

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav exposing (Key) 
import Url exposing (Url)
import Url.Parser as UP
import Maybe

type alias HasRouter r m c a = { a | router : Router r m c } 

type Command 
    = RouteRequested UrlRequest
    | UrlChanged Url

type alias RouteParser r = UP.Parser (r -> r) r

type alias Router route model cmd =
    { routeParser : RouteParser route
    , init : route -> (model, Cmd cmd)
    , notFound : route
    , navKey : Key
    }

-- High level api
handleRouting : HasRouter routes model cmd a -> Command -> model -> (model, Cmd cmd)
handleRouting { router } command model =
    case command of 
        RouteRequested request -> 
            (model, handleUrlRequest router.navKey request)
        UrlChanged url -> handleUrlChanged router url 

handleUrlChanged : Router route model cmd -> Url -> (model, Cmd cmd)
handleUrlChanged router url =
    UP.parse router.routeParser url
    |> Maybe.withDefault router.notFound
    |> router.init


handleUrlRequest : Key -> UrlRequest -> Cmd msg
handleUrlRequest navKey request = 
    case request of 
        Internal url -> Nav.pushUrl navKey <| Url.toString url
        External externalUrl -> Nav.load externalUrl
