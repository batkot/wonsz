module Dashboard 
    ( component
    ) where

import Prelude

import Halogen as H
import Halogen.HTML as HH

type State = Unit
type Action = Unit

component :: forall q i o m. H.Component q i o m
component =
    H.mkComponent 
        { initialState: \_ -> unit
        , render: render
        , eval: H.mkEval $ H.defaultEval
        }

render :: forall a s m. State -> H.ComponentHTML a s m
render _ = HH.text "Dashboard"
