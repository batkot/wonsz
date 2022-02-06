module Html.Extra exposing
    ( onKeyDown
    , onKey
    , KeyCode(..)

    , enter
    , escape

    , empty
    , spinner
    , errorIcon
    , questionIcon

    , exclamationMessage
    , questionMessage
    )

import Assets

import Html exposing (Html, Attribute, img, div, text)
import Html.Attributes exposing (src, class)
import Html.Events exposing (keyCode, on)

import Json.Decode as JD

type KeyCode = KeyCode Int

enter : KeyCode
enter = KeyCode 13

escape : KeyCode
escape = KeyCode 27

keyCodeToString : KeyCode -> String
keyCodeToString (KeyCode k) = "KeyCode: " ++ String.fromInt k

onKeyDown : (KeyCode -> msg) -> Attribute msg
onKeyDown f =
    onKeyDownInternal (\k -> JD.succeed (f k))

onKey : KeyCode -> msg -> Attribute msg
onKey key msg =
    let matchKey k =
            if k == key then JD.succeed msg
            else JD.fail ("Expected: " ++ keyCodeToString key ++ ". Got: " ++ keyCodeToString k)
    in onKeyDownInternal matchKey

onKeyDownInternal : (KeyCode -> JD.Decoder msg) -> Attribute msg
onKeyDownInternal f =
    on "keydown" (JD.andThen (KeyCode >> f) keyCode)

empty : Html a
empty = Html.text ""

spinner : Html a
spinner =
    img [ class "spinner"
        , src Assets.singleSnake ]
        []

errorIcon : Html a
errorIcon =
    img [ class "error-icon icon"
        , src Assets.exclamation
        ]
        []

questionIcon : Html a
questionIcon =
    img [ class "question-icon icon"
        , src Assets.question
        ]
        []

messageBox : Html a -> String -> Html a
messageBox icon message = 
    div [ class "messagebox" ]
        [ div 
            [ class "icon-container" ]
            [ icon ]
        , div 
            [ class "message-container" ]
            [ text message ]
        ]

exclamationMessage : String -> Html a
exclamationMessage = messageBox errorIcon

questionMessage : String -> Html a
questionMessage = messageBox questionIcon
