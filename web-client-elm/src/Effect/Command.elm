module Effect.Command exposing
    ( CommandFx(..)

    , runCommandFx
    , map
    )

import Cmd.Extra as CE
import Delay as D
import Time
import Task
import Process

type CommandFx msg
    = Raise msg
    | Delay msg Float
    | Schedule msg Int

map : (a -> b) -> CommandFx a -> CommandFx b
map f cmd =
    case cmd of
        Raise x -> Raise (f x)
        Delay x t -> Delay (f x) t
        Schedule x t -> Schedule (f x) t

runCommandFx : CommandFx msg -> Cmd msg
runCommandFx eff =
    case eff of
        Raise msg -> 
            CE.pure msg

        Delay msg minutes -> 
            D.after minutes D.Minute msg

        Schedule msg time -> 
            Time.now
            |> Task.map Time.posixToMillis
            |> Task.map ((-) time >> toFloat)
            |> Task.andThen Process.sleep
            |> Task.perform (always msg)
