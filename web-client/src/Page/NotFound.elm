module Page.NotFound exposing (view)

import Html exposing (Html, img, div, text)
import Html.Attributes exposing (src, class)

import Assets exposing (notFound)
import Lang exposing (Dict, HasDict)

view : HasDict a -> Html cmd
view { dict } =
    div []
        [ div
            [ class "not-found" ]
            [ img [ src notFound ] [] ]
        , text dict.notFoundMessage
        ]
