module Scoreboard exposing (testView)

import Html exposing (Html, div, text, img)
import Html.Attributes exposing (class, src)

import Html.Extra as HE

import Lang exposing (HasDict)

import Assets exposing (makkay, hub, btk, mateusz, kuba, szuro)

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
    [ Player "Paweł Machay" 12 1 makkay []
    , Player "Hubert Kotlarz" 10 2 hub []
    , Player "Tomek Batko" 8 3 btk []
    , Player "Kuba Dziedzic" 7 4 kuba [] 
    , Player "Paweł Szuro" 4 5 szuro []
    , Player "Mateusz Wałach" 3 6 mateusz []
    ]

testView : HasDict x -> Html a
testView d = view d players
       
view : HasDict x -> List Player -> Html a
view d p = 
    let leader = List.head p
            |> Maybe.map (playerView d)
            |> Maybe.withDefault HE.empty
        rest = List.tail p
            |> Maybe.withDefault []
        podium = rest
            |> List.take 2
            |> List.map (playerView d)
        suckers = rest
            |> List.drop 2
            |> List.map (playerView d)
    in div
        [ class "scoreboard" ]
        [ div 
            [ class "leader" ]
            [ leader ]
        , div 
            [ class "rest" ]
            podium
        , div
            [ class "suckers" ]
            suckers
        ]

placeView : Int -> Html a
placeView place = 
    div [ class "place", class (placeClass place) ] [ text (String.fromInt place) ]

placeClass : Int -> String
placeClass place = 
    let placeString = if place < 4 then String.fromInt place else "n"
    in "place-" ++ placeString

playerView : HasDict x -> Player -> Html a
playerView { dict } player = 
        div [ class "player", class (placeClass player.place) ]
            [ img [ src player.avatarUrl ] []
            , placeView player.place
            , div [ class "name" ] [ text player.name ]
            , div [ class "score" ] [ text (String.fromInt player.score ++ " " ++ dict.pointLabel )]
            ]
