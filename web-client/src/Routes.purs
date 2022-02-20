module Routes where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic as RDG
import Routing.Duplex.Generic.Syntax ((/))

data Route 
    = Dashboard
    | NotFound
    | Hey

derive instance genericRoute :: Generic Route _

codec :: RouteDuplex' Route
codec = RDG.sum
    { "Dashboard": RDG.noArgs
    , "NotFound": "not-found" / RDG.noArgs
    , "Hey": "hey" / RDG.noArgs
    }
