port module LocalStorage exposing 
    ( store
    , storeString
    , setLocalStorageKey
    )

import Json.Encode as JE

type alias Key = String

store : (a -> JE.Value) -> Key -> a -> Cmd msg
store encode k v = setLocalStorageKey (k, encode v)

storeString : Key -> String -> Cmd msg
storeString key = store JE.string key

port setLocalStorageKey : (String, JE.Value) -> Cmd msg
