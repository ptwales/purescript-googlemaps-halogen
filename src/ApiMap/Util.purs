module GMaps.ApiMap.Util
  ( loadMap
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Effect (Effect)
import GMaps.Map (Map, MapOptions, gMap) as GM
import Web.DOM (Element) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (toNonElementParentNode) as HTML
import Web.HTML.Window (document) as HTML

getElementById :: String -> Effect (Maybe DOM.Element)
getElementById idText = do
  window <- HTML.window
  document <- HTML.document window
  let
    documentNode = HTML.toNonElementParentNode document
  DOM.getElementById idText documentNode

loadMap :: String -> GM.MapOptions -> Effect (Maybe GM.Map)
loadMap anchorId mapOptions = do
  anchor <- getElementById anchorId
  traverse (flip GM.gMap mapOptions) anchor
