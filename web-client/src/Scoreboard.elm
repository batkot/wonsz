module Scoreboard exposing (testView)

import Html exposing (Html, div, text, img)
import Html.Attributes exposing (class, src)

import Html.Extra as HE

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
    , Player "Tomek Batko" 8 3 pawelMachay []
    , Player "Kuba Dziedzic" 7 4 pawelMachay [] 
    , Player "Paweł Szuro" 7 4 pawelMachay []
    , Player "Mateusz Wałach" 3 6 pawelMachay []
    ]

testView : Html a
testView = view players
       

view : List Player -> Html a
view p = 
    let leader = List.head p
            |> Maybe.map bigPlayerView
            |> Maybe.withDefault HE.empty
        rest = List.tail p
            |> Maybe.withDefault []
        podium = rest
            |> List.take 2
            |> List.map bigPlayerView
    in div
        [ class "scoreboard" ]
        [ div 
            [ class "leader" ]
            [ leader ]
        , div 
            [ class "rest" ]
            podium
        ]

bigPlayerView : Player -> Html a
bigPlayerView player = 
    div [ class "player-big" ]
        [ img [ src player.avatarUrl ] []
        , placeView player.place
        , div [ class "score" ] [ text (String.fromInt player.score ++ "pkt.") ]
        ]

placeView : Int -> Html a
placeView place = 
    div [ class "place", class (placeClass place) ] [ text (String.fromInt place) ]

placeClass : Int -> String
placeClass place = 
    let placeString = if place < 4 then String.fromInt place else "n"
    in "place-" ++ placeString

playerView : Player -> Html a
playerView player = 
        div [ class "player", class (placeClass player.place) ]
            [ placeView player.place
            , div [ class "name" ] [ text player.name ]
            , div [ class "score" ] [ text (String.fromInt player.score ++ " pkt." )]
            ]
