module Effect exposing
    ( Fx
    , map
    , pure
    , op
    , bind
    , pipe

    , addFx
    , mapFx
    , just
    , push

    , runFx
    )

-- Writer
type Fx eff a = Fx (List eff) a

map : (a -> b) -> Fx eff a -> Fx eff b
map f (Fx fx x) = Fx fx <| f x

pure : x -> Fx eff x
pure = Fx []

op : Fx eff (a -> b) -> Fx eff a -> Fx eff b
op (Fx fx1 f) (Fx fx2 x) = Fx (fx1 ++ fx2) <| f x

bind : Fx eff a -> (a -> Fx eff b) -> Fx eff b
bind (Fx fx1 a) f =
    let (Fx fx2 b) = f a
    in Fx (fx1 ++ fx2) b

pipe : Fx eff a -> Fx eff b -> Fx eff b
pipe a b = bind a <| always b

runFx : (eff -> Cmd msg) -> Fx eff a -> (a, Cmd msg)
runFx run (Fx effects value) =
    let cmds = List.map run effects
    in (value, Cmd.batch cmds)

addFx : eff -> a -> Fx eff a
addFx fx a = pure a
    |> push fx

just : eff -> Fx eff ()
just fx = Fx [fx] ()

push : eff -> Fx eff a -> Fx eff a
push fx (Fx x a) = Fx (x ++ [fx]) a

mapFx : (a -> b) -> Fx a x -> Fx b x
mapFx f (Fx fx x) = Fx (List.map f fx) x
