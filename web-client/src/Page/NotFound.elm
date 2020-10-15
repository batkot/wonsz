module Page.NotFound exposing (view)

import Html.Extra exposing (spinner)
import Html exposing (Html, img, div)
import Html.Attributes exposing (src, class)

import Assets exposing (notFound)

view : Html a
view = 
    div []
        [ div 
            [ class "not-found" ]
            [ img [ src notFound ] [] ]
        , spinner
        ]
