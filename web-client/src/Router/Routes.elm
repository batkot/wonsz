module Router.Routes exposing
    ( Route(..)
    , routes

    , parseUrl
    , toUrl
    )

import Url.Builder exposing (relative)
import Url.Parser as UP exposing ((</>))
import Url exposing (Url)

type Route
    = NotFound
    | Account Int
    | Dashboard

routes : UP.Parser (Route -> a) a
routes = UP.oneOf
    [ UP.map Account (UP.s "account" </> UP.int)
    , UP.map Dashboard UP.top
    ]

parseUrl : Url -> Route
parseUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
    |> UP.parse routes
    |> Maybe.withDefault NotFound

toUrl : Route -> String
toUrl r =
    let
        buildStringUrl = String.append "/#"
    in
        case r of
            NotFound -> ""
            Account id ->
                relative ["account", String.fromInt id] []
                |> buildStringUrl
            Dashboard -> ""
