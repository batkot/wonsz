module Effect exposing
    (..)

import Http
import Http.Extra as HE

import Cmd.Extra as CE
import Delay as D

import IO.LocalStorage as LS

    
interpret : Cmd msg -> (eff -> Cmd msg) -> Eff eff a -> (a, Cmd msg)
interpret return runFx fx = 
    case fx of
        Pure x -> (x, return)
        Effect (eff, next) -> 
            let (x, nextCmd) = interpret return runFx next
                cmd = runFx eff
            in (x, Cmd.batch [cmd, nextCmd])

type CommandFx msg 
    = Raise msg
    | Delay msg Float

type HttpFx a msg
    = Request (HE.HttpRequest a) (Result Http.Error a -> msg)

type LocalStorageFx 
    = Store String String 
    | Clear String

runLocalStorageFx : LocalStorageFx -> Cmd msg
runLocalStorageFx eff =
    case eff of
        Store key value -> LS.storeString key value
        Clear key -> LS.clearKey key

runHttpFx : HE.Url -> HttpFx a msg -> Cmd msg
runHttpFx baseUrl eff = 
    case eff of
        Request request f -> 
            HE.execute baseUrl request
            |> Cmd.map f

runCommandFx : CommandFx msg -> Cmd msg
runCommandFx eff = 
    case eff of 
        Raise msg -> CE.pure msg
        Delay msg minutes -> D.after minutes D.Minute msg

type Comp a b 
    = A a 
    | B b

runCompFx : (a -> Cmd msg) -> (b -> Cmd msg) -> Comp a b -> Cmd msg
runCompFx fa fb comp =
    case comp of
        A a -> fa a
        B b -> fb b

type Eff eff a 
    = Pure a
    | Effect (eff, Eff eff a)

combine : Eff eff1 a -> Eff eff2 a -> Eff (Comp eff1 eff2) a
combine first second = 
    let a = mapFx A first
        b = mapFx B second
    in bind a (always b)

bind : Eff fx a -> (a -> Eff fx b) -> Eff fx b
bind fx f =
    case fx of
        Pure a -> f a
        Effect (eff, i) -> Effect (eff, bind i f)

pure : a -> Eff eff a
pure = Pure

addFx: eff -> Eff eff a -> Eff eff a
addFx new fx = Effect (new, fx)

mapFx : (eff -> eff2) -> Eff eff a -> Eff eff2 a
mapFx f eff =
    case eff of
        Pure x -> Pure x
        Effect (fx, i) -> Effect (f fx, mapFx f i)

fMap : (a -> b) -> Eff eff a -> Eff eff b
fMap f eff = 
    case eff of
        Pure x -> Pure (f x)
        Effect (fx, i) -> Effect (fx, fMap f i)

store : String -> String -> Eff LocalStorageFx ()
store key value = 
    pure ()
    |> addFx (Store key value)
