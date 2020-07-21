module Scoreboard exposing (testView)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

import Assets exposing (pawelMachay)

type alias Player =
    { name : String
    , score : Int
    , place : Int
    , avatarUrl : String
    , points : List Point
    }

type alias Point =
    { date : String
    }

players : List Player
players =
    [ Player "Paweł Machay" 12 1 pawelMachay []
    , Player "Hubert Kotlarz" 10 2 pawelMachay []
    , Player "Tomasz Batko" 8 3 pawelMachay []
    , Player "Kuba Dziedzic" 7 4 pawelMachay [] 
    , Player "Paweł Szuro" 7 4 pawelMachay []
    , Player "Mateusz Wałach" 3 6 pawelMachay []
    ]

testView : Html a
testView = view players
       

view : List Player -> Html a
view p = div
    [ class "scoreboard" ]
    <| List.map playerView p

playerView : Player -> Html a
playerView player = 
    let 
        placeClass place = if place < 4 then String.fromInt place else "n"
    in 
        div [ class "player", class ("place-" ++ placeClass player.place) ]
            [ div [ class "place" ] [ text (String.fromInt player.place) ]
            , div [ class "name" ] [ text player.name ]
            , div [ class "score" ] [ text (String.fromInt player.score ++ " pkt." )]
            ]
