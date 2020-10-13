module Page.NotFound exposing (view)

import Html exposing (Html, img, div)
import Html.Attributes exposing (src, class)

import Assets exposing (notFound)

view : Html a
view = 
    div [ class "not-found" ]
        [ img [ src notFound ] []
        ]
