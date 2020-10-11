module Effect.LocalStorage exposing
    ( LocalStorageFx(..)
    , runLocalStorageFx
    )

import IO.LocalStorage as LS

type LocalStorageFx 
    = Store String String 
    | Clear String

runLocalStorageFx : LocalStorageFx -> Cmd msg
runLocalStorageFx eff =
    case eff of
        Store key value -> LS.storeString key value
        Clear key -> LS.clearKey key
