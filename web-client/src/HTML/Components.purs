module HTML.Components where

import Assets (Assets)

import Halogen.HTML (img, ClassName(..))
import Halogen.HTML.Core (HTML)
import Halogen.HTML.Properties (src, class_)

spinner :: forall w i. Assets -> HTML w i
spinner assets = 
    img [ src assets.singleSnakeUrl 
        , class_ (ClassName "spinner") ]
