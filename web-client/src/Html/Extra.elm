module Html.Extra exposing
    ( onKeyDown
    , onKey
    , KeyCode(..)

    , enter
    )

import Html exposing (Attribute)
import Html.Events exposing (keyCode, on)

import Json.Decode as JD


type KeyCode = KeyCode Int

enter : KeyCode
enter = KeyCode 13

onKeyDown : (KeyCode -> msg) -> Attribute msg
onKeyDown f = 
    onKeyDownInternal (\k -> JD.succeed (f k))

onKey : KeyCode -> msg -> Attribute msg
onKey key msg =
    onKeyDownInternal (\k -> if k == key then JD.succeed msg else JD.fail "")

onKeyDownInternal : (KeyCode -> JD.Decoder msg) -> Attribute msg
onKeyDownInternal f = 
    on "keydown" (JD.andThen (KeyCode >> f) keyCode)

