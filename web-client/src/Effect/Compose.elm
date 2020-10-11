module Effect.Compose exposing
    ( FxComp

    , runFxComp
    , pushLeft
    , pushRight
    , next
    , here

    , bimap
    , mapLeft
    , mapRight
    )

import Effect as Fx exposing (Fx)

type FxComp a b 
    = L a
    | R b

next : b -> FxComp a b
next = R

here : a -> FxComp a b
here = L

runFxComp : (a -> Cmd msg) -> (b -> Cmd msg) -> FxComp a b -> Cmd msg
runFxComp f g comp = 
    case comp of
        L a -> f a
        R b -> g b

bimap : (a -> c) -> (b -> d) -> FxComp a b -> FxComp c d
bimap f g comp = 
    case comp of
        L x -> L (f x)
        R y -> R (g y)

mapLeft : (a -> b) -> FxComp a x -> FxComp b x
mapLeft f = bimap f identity

mapRight : (a -> b) -> FxComp x a -> FxComp x b
mapRight = bimap identity

pushRight : effR -> Fx effL a -> Fx (FxComp effL effR) a
pushRight fx eff = 
    Fx.mapFx here eff
    |> Fx.push (next fx)

pushLeft : effL -> Fx effR a -> Fx (FxComp effL effR) a
pushLeft fx eff =
    Fx.mapFx next eff
    |> Fx.push (here fx)
