port module IO.LocalStorage exposing
    ( store
    , storeString
    , setLocalStorageKey

    , clearKey
    , dropLocalStorageKey

    , storageKeyChanged
    , listenStorageKeyChange
    , listenStringStorageKeyChange
    )

import Json.Encode as JE
import Json.Decode as JD

type alias Key = String

port setLocalStorageKey : (String, JE.Value) -> Cmd msg

store : (a -> JE.Value) -> Key -> a -> Cmd msg
store encode k v = setLocalStorageKey (k, encode v)

storeString : Key -> String -> Cmd msg
storeString key = store JE.string key

port dropLocalStorageKey : String -> Cmd msg

clearKey : Key -> Cmd msg
clearKey = dropLocalStorageKey

port storageKeyChanged : ((String, JE.Value) -> msg) -> Sub msg

listenStorageKeyChange : JD.Decoder a -> Key -> Sub (Maybe a)
listenStorageKeyChange decoder key =
    let match (k, json) =
            if k == key then
                case JD.decodeValue decoder json of
                    Ok cmd -> Just cmd
                    Err _ -> Nothing
            else Nothing
    in storageKeyChanged match

listenStringStorageKeyChange : Key -> Sub (Maybe String)
listenStringStorageKeyChange = listenStorageKeyChange JD.string
