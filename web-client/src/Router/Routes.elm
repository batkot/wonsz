module Router.Routes exposing
    ( Route(..)
    , routes
    )

import Url.Parser as UP exposing ((</>)) 

type Route 
    = NotFound
    | Account Int

routes : UP.Parser (Route -> a) a
routes = UP.oneOf
    [ UP.map Account (UP.s "account" </> UP.int)
    ]
