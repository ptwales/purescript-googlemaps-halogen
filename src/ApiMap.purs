module GMaps.ApiMap
  ( Input
  , MarkerInput
  , Output
  , PolygonInput
  , PolylineInput
  , Query
  , Slot
  , component
  , anchorId
  ) where

import Prelude
import Data.Array (fromFoldable, singleton) as Array
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import GMaps.ApiMap.Marker as Marker
import GMaps.ApiMap.Polygon as Polygon
import GMaps.ApiMap.Polyline as Polyline
import GMaps.ApiMap.Util (loadMap)
import GMaps.Draw.Polygon (PolygonOptions) as GM
import GMaps.Draw.Polyline (PolylineOptions) as GM
import GMaps.InfoWindow (InfoWindowOptions) as GM
import GMaps.Map (Map, MapOptions) as GM
import GMaps.Marker (MarkerOptions) as GM
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type MarkerInput
  = { key :: Marker.Key
    , options :: GM.MarkerOptions
    , infoWindow :: Maybe GM.InfoWindowOptions
    }

type PolygonInput
  = { key :: Polygon.Key
    , options :: GM.PolygonOptions
    }

type PolylineInput
  = { key :: Polyline.Key
    , options :: GM.PolylineOptions
    }

type State
  = { gmap :: Maybe GM.Map
    , mapOptions :: GM.MapOptions
    , markers :: Array MarkerInput
    , polygons :: Array PolygonInput
    , polylines :: Array PolylineInput
    }

type Input
  = { mapOptions :: GM.MapOptions
    , markers :: Array MarkerInput
    , polygons :: Array PolygonInput
    , polylines :: Array PolylineInput
    }

data Output
  = MarkerMessage Marker.Key Marker.Output
  | PolygonMessage Polygon.Key Polygon.Output
  | PolylineMessage Polyline.Key Polyline.Output

data Action
  = Init
  | Update Input
  | MarkerEvent Marker.Key Marker.Output
  | PolygonEvent Polygon.Key Polygon.Output
  | PolylineEvent Polyline.Key Polyline.Output

data Query a
  = PolygonQuery Polygon.Key (Polygon.Query a)
  | PolylineQuery Polyline.Key (Polyline.Query a)

type Slot p
  = H.Slot Query Output p

type ChildSlots
  = ( marker :: Marker.Slot
    , polygon :: Polygon.Slot
    , polyline :: Polyline.Slot
    )

_marker :: SProxy "marker"
_marker = SProxy

_polygon :: SProxy "polygon"
_polygon = SProxy

_polyline :: SProxy "polyline"
_polyline = SProxy

type Render m
  = H.ComponentHTML Action ChildSlots m

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              , receive = Just <<< Update
              , initialize = Just Init
              , finalize = Nothing
              }
    }

initialState :: Input -> State
initialState input =
  { mapOptions: input.mapOptions
  , markers: input.markers
  , polygons: input.polygons
  , polylines: input.polylines
  , gmap: Nothing
  }

anchorId :: String
anchorId = "gmap-ip-anchor"

placeHolder :: HH.PlainHTML
placeHolder = HH.text "ERROR"

anchorHTML :: HH.PlainHTML -> HH.PlainHTML
anchorHTML = HH.div props <<< Array.singleton
  where
  props = [ HP.id_ anchorId, HP.class_ (HH.ClassName "google-map") ]

render :: forall m. MonadAff m => State -> Render m
render state =
  HH.div_
    $ [ HH.fromPlainHTML (anchorHTML placeHolder) ]
    <> (Array.fromFoldable state.gmap >>= renderChildren state)

renderChildren :: forall m. MonadAff m => State -> GM.Map -> Array (Render m)
renderChildren { markers, polygons, polylines } gmap =
  (renderMarker gmap <$> markers)
    <> (renderPolygon gmap <$> polygons)
    <> (renderPolyline gmap <$> polylines)

renderMarker :: forall m. MonadAff m => GM.Map -> MarkerInput -> Render m
renderMarker parent { key, options, infoWindow } =
  let
    input = { options: options { map = Just parent }, infoWindow, parent }

    listen message = Just (MarkerEvent key message)
  in
    HH.slot _marker key Marker.component input listen

renderPolygon :: forall m. MonadAff m => GM.Map -> PolygonInput -> Render m
renderPolygon parent { key, options } =
  let
    input = { options: options { map = Just parent }, parent }

    listen message = Just (PolygonEvent key message)
  in
    HH.slot _polygon key Polygon.component input listen

renderPolyline :: forall m. MonadAff m => GM.Map -> PolylineInput -> Render m
renderPolyline parent { key, options } =
  let
    input = { options: options { map = Just parent }, parent }

    listen message = Just (PolylineEvent key message)
  in
    HH.slot _polyline key Polyline.component input listen

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  Init -> do
    mapOptions <- H.gets _.mapOptions
    gmap <- liftEffect (loadMap anchorId mapOptions)
    H.modify_ (_ { gmap = gmap })
  Update { markers, polygons } -> do
    H.modify_
      ( _
          { markers = markers
          , polygons = polygons
          }
      )
  MarkerEvent markerKey message -> H.raise (MarkerMessage markerKey message)
  PolygonEvent polygonKey message -> H.raise (PolygonMessage polygonKey message)
  PolylineEvent polylineKey message -> H.raise (PolylineMessage polylineKey message)

handleQuery :: forall m a. Query a -> H.HalogenM State Action ChildSlots Output m (Maybe a)
handleQuery = case _ of
  PolygonQuery polygonKey subQuery -> H.query _polygon polygonKey subQuery
  PolylineQuery polylineKey subQuery -> H.query _polyline polylineKey subQuery
