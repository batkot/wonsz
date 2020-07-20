module Cmd.Extra exposing
    ( pure
    )

import Task

pure : a -> Cmd a
pure = Task.succeed >> Task.perform identity
