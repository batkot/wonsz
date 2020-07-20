module Scoreboard exposing (testView)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

type alias Player =
    { name : String
    , points : Int
    , place : Int
    }

players : List Player
players =
    [ Player "Paweł Machay" 12 1
    , Player "Hubert Kotlarz" 10 2
    , Player "Tomasz Batko" 8 3
    , Player "Kuba Dziedzic" 7 4
    , Player "Paweł Szuro" 7 4
    , Player "Mateusz Wałach" 3 6
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
            , div [ class "points" ] [ text (String.fromInt player.points ++ " pkt." )]
            ]
