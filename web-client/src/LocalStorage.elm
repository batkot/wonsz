port module LocalStorage exposing (store, setLocalStorageKey)

import Json.Encode as JE

type Key = String

store : (a -> JE.Value) -> String -> a -> Cmd msg
store encode k v = setLocalStorageKey (k, encode v)

port setLocalStorageKey : (String, JE.Value) -> Cmd msg
