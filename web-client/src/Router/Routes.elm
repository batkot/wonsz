module Router.Routes exposing
    ( Route(..)
    , routes

    , parseUrl
    )

import Url.Parser as UP exposing ((</>)) 
import Url exposing (Url)

type Route 
    = NotFound
    | Account Int

routes : UP.Parser (Route -> a) a
routes = UP.oneOf
    [ UP.map Account (UP.s "account" </> UP.int)
    , UP.map (Account 1) (UP.s "dupa")
    ]

parseUrl : Url -> Route
parseUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
    |> UP.parse routes 
    |> Maybe.withDefault NotFound
