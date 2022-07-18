module Routes where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', rest) 
import Routing.Duplex.Generic as RDG
import Routing.Duplex.Generic.Syntax ((/))

type Path = String

data Route 
    = Dashboard
    | NotFound (Array Path)

derive instance genericRoute :: Generic Route _

codec :: RouteDuplex' Route
codec = RDG.sum
    { "Dashboard": RDG.noArgs
    , "NotFound": rest
    }
