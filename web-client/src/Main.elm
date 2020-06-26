module Main exposing (main)

import Browser

import Html exposing (Html, text, img, div)
import Html.Attributes exposing (src, class)

import Assets exposing (elmLogoUrl)

main : Program Environment Model Command
main = 
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }

type alias Environment =
    { apiUrl : String
    }

init : Environment -> (Model, Cmd Command)
init _ = ({}, Cmd.none)

update : Command -> Model -> (Model, Cmd Command)
update _ m = (m, Cmd.none)

type alias Model = {}

type Command = Command

view : Model -> Html a
view _ = div 
    [ class "elm-container" ]
    [ img [ src elmLogoUrl ] []
    , text "Elm 0.19 Webpack4 Starter" 
    ]
