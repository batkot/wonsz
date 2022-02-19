module Main 
    ( main ) where

import Prelude

import App (runAppT)
import Assets (Assets)
import SignIn (component)
import Dict (Dict)
import Dict.EN as EN
import Dict.PL as PL
import Data.Argonaut (Json, decodeJson, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Foldable (sequence_)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Halogen as H
import Halogen.Aff as HA
import IO.Api as Api
import Halogen.VDom.Driver (runUI)
import Web.DOM.ChildNode as CN
import Web.DOM.Element as DE
import Web.DOM.HTMLCollection as HC
import Web.DOM.ParentNode as PN
import Web.HTML.HTMLElement (toParentNode)

type AppOptions =
    { appContainerSelector :: String
    , assets :: Assets
    , language :: String
    , apiUrl :: String
    }

main :: Json -> Effect Unit
main optionJson = 
    case decodeJson optionJson of
        Left errors -> EC.log $ printJsonDecodeError errors
        Right options -> runApp options

runApp :: AppOptions -> Effect Unit
runApp options = HA.runHalogenAff do
    body <- HA.awaitBody
    appContainer <- fromMaybe body <$> HA.selectElement (PN.QuerySelector options.appContainerSelector)
    liftEffect $ removeChildren (toParentNode appContainer)
    let dict = matchDict options.language
        appConfig = { apiUrl: Api.ApiUrl options.apiUrl }
        c = H.hoist (runAppT appConfig) $ component dict options.assets
    runUI c unit appContainer

removeChildren :: PN.ParentNode -> Effect Unit
removeChildren parent = do
    children <- PN.children parent >>= HC.toArray
    sequence_ $ map (DE.toChildNode >>> CN.remove)  children

matchDict :: String -> Dict
matchDict "PL" = PL.dict
matchDict "pl" = PL.dict
matchDict _ = EN.dict
