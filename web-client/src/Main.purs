module Main 
    ( main ) where

import Prelude

import App (runAppT)
import Assets (Assets)
import Dict (Dict)
import Dict.EN as EN
import Dict.PL as PL
import Data.Argonaut (Json, decodeJson, printJsonDecodeError)
import Data.Either (Either(..), hush)
import Data.Foldable (sequence_)
import Data.Maybe (fromMaybe, Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as EC
import Halogen as H
import Halogen.Aff as HA
import IO.Api as Api
import Routing.Hash (matchesWith, getHash)
import Routing.Duplex (parse, print)
import Routes as R
import Root as Root
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
    hash <- liftEffect getHash
    liftEffect $ EC.log $ "Hash: " <> hash
    startRoute <- liftEffect $ fromMaybe R.NotFound <<< hush <<< parse R.codec <$> getHash
    liftEffect $ EC.log $ "Route: " <> (print R.codec startRoute)
    let dict = matchDict options.language
        appConfig = { apiUrl: Api.ApiUrl options.apiUrl }
        c = H.hoist (runAppT appConfig) $ Root.component startRoute dict options.assets
    appIO <- runUI c unit appContainer
    void $ liftEffect $ matchesWith (parse R.codec) $ \_ new -> 
        launchAff_ $ appIO.query $ H.mkTell $ Root.Navigate new

removeChildren :: PN.ParentNode -> Effect Unit
removeChildren parent = do
    children <- PN.children parent >>= HC.toArray
    sequence_ $ map (DE.toChildNode >>> CN.remove)  children

matchDict :: String -> Dict
matchDict "PL" = PL.dict
matchDict "pl" = PL.dict
matchDict _ = EN.dict
