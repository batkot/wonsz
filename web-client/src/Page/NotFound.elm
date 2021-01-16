module Page.NotFound exposing (view)

import Html exposing (Html)

import Html.Extra exposing (questionMessage)

import Lang exposing (HasDict)

view : HasDict a -> Html cmd
view { dict } = questionMessage dict.notFoundMessage
