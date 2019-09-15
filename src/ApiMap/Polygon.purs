module GMaps.ApiMap.Polygon
  ( Key
  , Output
  , Query
  , Slot
  , component
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Traversable (for, for_)
import Effect.Aff.Class (class MonadAff)
import GMaps.ApiMap.Event (eventSource)
import GMaps.Draw.Polygon (Polygon, PolygonOptions, deletePolygon, newPolygon, remove, setOptions) as GM
import GMaps.Draw.Polygon.PolygonEvent (PolygonEvent(..)) as GM
import GMaps.Geometry.Poly (containsLocation, isOnEdgeOfPolygon) as GM
import GMaps.LatLng (LatLng)
import GMaps.MVC.MouseEvent (MVCMouseEvent(..)) as GM
import GMaps.MVC.PolyMouseEvent (PolyMouseEvent) as GM
import GMaps.Map (Map) as GM
import Halogen as H
import Halogen.HTML as HH

type Key
  = String

type State
  = { options :: GM.PolygonOptions
    , polygon :: Maybe GM.Polygon
    , parent :: GM.Map
    , subscriptions :: Array H.SubscriptionId
    }

type Input
  = { options :: GM.PolygonOptions
    , parent :: GM.Map
    }

data Output
  = Message GM.PolygonEvent GM.PolyMouseEvent

data Query a
  = Contains LatLng (Boolean -> a)
  | IsOnEdge LatLng (Boolean -> a)

data Action
  = Load
  | Update Input
  | Remove
  | Event GM.PolygonEvent GM.PolyMouseEvent

type Slot
  = H.Slot Query Output Key

component :: forall m. MonadAff m => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState
    , render: const render
    , eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            , receive = Just <<< Update
            , initialize = Just Load
            , finalize = Just Remove
            }
    }

initialState :: Input -> State
initialState { options, parent } =
  { options
  , parent
  , polygon: Nothing
  , subscriptions: []
  }

render :: forall act m. H.ComponentHTML act () m
render = HH.div_ []

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Load -> do
    state <- H.get
    let
      parent = state.parent

      options = state.options { map = Just parent }
    created <- H.liftEffect (GM.newPolygon options)
    sids <- for (eventSource created Event <$> events) H.subscribe
    H.put
      $ state
          { options = options
          , polygon = Just created
          , subscriptions = sids
          }
  Update { options, parent } -> do
    state <- H.get
    let
      update = flip GM.setOptions options
    polygon <- H.liftEffect (for state.polygon update)
    H.put
      $ state
          { polygon = polygon
          , options = options
          , parent = parent
          }
  Remove -> do
    state <- H.get
    let
      options = state.options { map = Nothing }
    removed <- H.liftEffect (for state.polygon GM.remove)
    H.liftEffect (for_ removed GM.deletePolygon)
    H.put
      $ state
          { polygon = Nothing
          , options = options
          }
  Event mvcEvent polyMouseEvent -> do
    H.raise (Message mvcEvent polyMouseEvent)

events :: Array GM.PolygonEvent
events =
  GM.PolygonEvent
    <$> [ GM.Click
      , GM.DblClick
      , GM.Drag
      , GM.DragEnd
      , GM.DragStart
      , GM.MouseDown
      , GM.MouseOut
      , GM.MouseOver
      , GM.MouseUp
      , GM.RightClick
      ]

handleQuery :: forall m a. Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  Contains latLng k -> do
    polygon <- H.gets _.polygon
    pure (k <<< GM.containsLocation latLng <$> polygon)
  IsOnEdge latLng k -> do
    polygon <- H.gets _.polygon
    pure (k <<< GM.isOnEdgeOfPolygon latLng <$> polygon)
