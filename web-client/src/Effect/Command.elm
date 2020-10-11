module Effect.Command exposing
    ( CommandFx(..)

    , runCommandFx
    )

import Cmd.Extra as CE
import Delay as D

type CommandFx msg
    = Raise msg
    | Delay msg Float

runCommandFx : CommandFx msg -> Cmd msg
runCommandFx eff = 
    case eff of 
        Raise msg -> CE.pure msg
        Delay msg minutes -> D.after minutes D.Minute msg
