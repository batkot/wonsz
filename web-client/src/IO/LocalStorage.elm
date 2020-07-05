port module IO.LocalStorage exposing 
    ( store
    , storeString
    , setLocalStorageKey

    , clearKey
    , dropLocalStorageKey
    )

import Json.Encode as JE

type alias Key = String

store : (a -> JE.Value) -> Key -> a -> Cmd msg
store encode k v = setLocalStorageKey (k, encode v)

storeString : Key -> String -> Cmd msg
storeString key = store JE.string key

port setLocalStorageKey : (String, JE.Value) -> Cmd msg
port dropLocalStorageKey : String -> Cmd msg

clearKey : Key -> Cmd msg
clearKey = dropLocalStorageKey
