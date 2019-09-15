module GMaps.ApiMap.Marker
  ( Key
  , Output
  , Slot
  , component
  ) where

import Prelude
import Control.Monad.State.Class (class MonadState)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (for, for_)
import Effect.Aff.Class (class MonadAff)
import GMaps.ApiMap.Event (eventSource)
import GMaps.ApiMap.InfoWindow as InfoWindow
import GMaps.InfoWindow (InfoWindowOptions) as GM
import GMaps.InfoWindow.InfoWindowEvent (InfoWindowEvent(..)) as InfoWindowEvent
import GMaps.InfoWindow.InfoWindowEvent (InfoWindowEvent)
import GMaps.InfoWindow.InfoWindowEvent (InfoWindowEvent) as GM
import GMaps.MVC.MouseEvent (MouseEvent, MVCMouseEvent(..)) as GM
import GMaps.Map (Map) as GM
import GMaps.Marker (Marker, MarkerOptions, deleteMarker, removeMarker, newMarker, setOptions) as GM
import GMaps.Marker.MarkerEvent (MarkerEvent(..)) as GM
import Halogen as H
import Halogen.HTML as HH

type Key
  = String

type State
  = { options :: GM.MarkerOptions
    , infoWindow :: Maybe GM.InfoWindowOptions
    , marker :: Maybe GM.Marker
    , parent :: GM.Map
    , subscriptions :: Array H.SubscriptionId
    }

type Input
  = { options :: GM.MarkerOptions
    , infoWindow :: Maybe GM.InfoWindowOptions
    , parent :: GM.Map
    }

data Output
  = Message GM.MarkerEvent GM.MouseEvent
  | InfoWindowMessage GM.InfoWindowEvent

data Action
  = Load
  | Update Input
  | Remove
  | Event GM.MarkerEvent GM.MouseEvent
  | InfoWindowEvent GM.InfoWindowEvent

type Slot
  = forall q. H.Slot q Output Key

type ChildSlots
  = ( infoWindow :: InfoWindow.Slot Unit
    )

_infoWindow :: SProxy "infoWindow"
_infoWindow = SProxy

component :: forall f m. MonadAff m => H.Component HH.HTML f Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
      H.mkEval
        $ H.defaultEval
            { handleAction = handleAction
            , receive = Just <<< Update
            , initialize = Just Load
            , finalize = Just Remove
            }
    }

initialState :: Input -> State
initialState { options, infoWindow, parent } =
  { options
  , infoWindow
  , parent
  , marker: Nothing
  , subscriptions: []
  }

type Render m
  = H.ComponentHTML Action ChildSlots m

render :: forall m. MonadAff m => State -> Render m
render state =
  HH.div_
    $ fromMaybe [] do
        marker <- state.marker
        infoWindowOptions <- state.infoWindow
        pure [ renderInfoWindow state.parent marker infoWindowOptions ]

renderInfoWindow :: forall m. MonadAff m => GM.Map -> GM.Marker -> GM.InfoWindowOptions -> Render m
renderInfoWindow gmap marker options =
  let
    parent = { gmap, marker }

    input = { parent, options }

    listen (InfoWindow.Message mvcEvent) = Just (InfoWindowEvent mvcEvent)
  in
    HH.slot _infoWindow unit InfoWindow.component input listen

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  Load -> do
    state <- H.get
    let
      options = state.options { map = Just state.parent }
    created <- H.liftEffect (GM.newMarker options)
    sids <- for (eventSource created Event <$> events) H.subscribe
    H.put
      $ state
          { marker = Just created
          , options = options
          , subscriptions = sids
          }
  Update { options, infoWindow, parent } -> do
    state <- H.get
    let
      update = flip GM.setOptions options
    marker <- H.liftEffect (for state.marker update)
    H.put
      $ state
          { marker = marker
          , options = options
          , infoWindow = infoWindow
          , parent = parent
          }
  Remove -> do
    state <- H.get
    let
      options = state.options { map = Nothing }
    removed <- H.liftEffect (for state.marker GM.removeMarker)
    H.liftEffect (for_ removed GM.deleteMarker)
    H.put (state { marker = Nothing, options = options })
  Event mvcEvent mouseEvent -> do
    H.raise (Message mvcEvent mouseEvent)
  InfoWindowEvent mvcEvent -> do
    evalInfoWindow mvcEvent
    H.raise (InfoWindowMessage mvcEvent)

evalInfoWindow :: forall m. MonadState State m => InfoWindowEvent -> m Unit
evalInfoWindow = case _ of
  InfoWindowEvent.CloseClick -> H.modify_ (_ { infoWindow = Nothing })
  _ -> pure unit

events :: Array GM.MarkerEvent
events =
  GM.MarkerEvent
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
