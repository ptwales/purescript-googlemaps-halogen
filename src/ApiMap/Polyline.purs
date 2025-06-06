module GMaps.ApiMap.Polyline
  ( Input
  , Key
  , Output(..)
  , Query(..)
  , Slot
  , component
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Traversable (for, for_)
import Effect.Aff.Class (class MonadAff)
import GMaps.ApiMap.Event (listenToAll)
import GMaps.Draw.Polyline (Polyline, PolylineOptions, deletePolyline, setOptions, newPolyline, remove) as GM
import GMaps.Draw.Polyline.PolylineEvent (PolylineEvent(..)) as GM
import GMaps.Geometry.Poly (isOnEdgeOfPolyline) as GM
import GMaps.LatLng (LatLng)
import GMaps.MVC.MouseEvent (MVCMouseEvent(..)) as GM
import GMaps.MVC.PolyMouseEvent (PolyMouseEvent) as GM
import GMaps.Map (Map) as GM
import Halogen as H
import Halogen.HTML as HH
import Halogen.Subscription as HS

type Key = String

type State =
  { options :: GM.PolylineOptions
  , polyline :: Maybe GM.Polyline
  , parent :: GM.Map
  , subscription :: Maybe H.SubscriptionId
  }

type Input =
  { options :: GM.PolylineOptions
  , parent :: GM.Map
  }

data Output = Message GM.PolylineEvent GM.PolyMouseEvent

data Query a = IsOnEdge LatLng (Boolean -> a)

data Action
  = Load
  | Update Input
  | Remove
  | Event GM.PolylineEvent GM.PolyMouseEvent

type Slot = H.Slot Query Output Key

component :: forall m. MonadAff m => H.Component Query Input Output m
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
  , polyline: Nothing
  , subscription: Nothing
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
    created <- H.liftEffect (GM.newPolyline options)
    { emitter, listener } <- H.liftEffect HS.create
    sid <- H.subscribe emitter
    H.liftEffect $ listenToAll created listener Event events
    H.put
      $ state
          { options = options
          , polyline = Just created
          , subscription = Just sid
          }
  Update { options, parent } -> do
    state <- H.get
    let
      update = flip GM.setOptions options
    polyline <- H.liftEffect (for state.polyline update)
    H.put
      $ state
          { polyline = polyline
          , options = options
          , parent = parent
          }
  Remove -> do
    state <- H.get
    let
      options = state.options { map = Nothing }
    removed <- H.liftEffect (for state.polyline GM.remove)
    for_ state.subscription H.unsubscribe
    H.liftEffect (for_ removed GM.deletePolyline)
    H.put
      $ state
          { polyline = Nothing
          , options = options
          }
  Event mvcEvent polyMouseEvent -> do
    H.raise (Message mvcEvent polyMouseEvent)

events :: Array GM.PolylineEvent
events =
  GM.PolylineEvent
    <$>
      [ GM.Click
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
  IsOnEdge latLng k -> do
    polyline <- H.gets _.polyline
    pure (k <<< GM.isOnEdgeOfPolyline latLng <$> polyline)
