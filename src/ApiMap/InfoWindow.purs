module GMaps.ApiMap.InfoWindow
  ( Input
  , Output(..)
  , Parent
  , Slot
  , component
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Traversable (for, for_)
import Effect.Aff.Class (class MonadAff)
import GMaps.ApiMap.Event (listenToAll)
import GMaps.InfoWindow (InfoWindow, InfoWindowOptions, closeInfoWindow, deleteInfoWindow, newInfoWindow, openInfoWindow, setOptions) as GM
import GMaps.InfoWindow.InfoWindowEvent (InfoWindowEvent(..)) as GM
import GMaps.Map (Map) as GM
import GMaps.Marker (Marker) as GM
import Halogen as H
import Halogen.HTML as HH
import Halogen.Subscription as HS

type Parent =
  { gmap :: GM.Map
  , marker :: GM.Marker
  }

type State =
  { options :: GM.InfoWindowOptions
  , parent :: Parent
  , infoWindow :: Maybe GM.InfoWindow
  , subscription :: Maybe H.SubscriptionId
  }

type Input =
  { options :: GM.InfoWindowOptions
  , parent :: Parent
  }

data Output = Message GM.InfoWindowEvent

data Action
  = Load
  | Update Input
  | Event GM.InfoWindowEvent Unit
  | Remove

type Slot p = forall q. H.Slot q Output p

component :: forall f m. MonadAff m => H.Component f Input Output m
component =
  H.mkComponent
    { initialState
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Load
              , finalize = Just Remove
              , receive = Just <<< Update
              }
    , render: const render
    }

initialState :: Input -> State
initialState { options, parent } =
  { options
  , parent
  , infoWindow: Nothing
  , subscription: Nothing
  }

render :: forall act m. H.ComponentHTML act () m
render = HH.div_ []

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Load -> do
    state <- H.get
    let
      { options, parent: { gmap, marker } } = state
    created <- H.liftEffect (GM.newInfoWindow options)
    opened <- H.liftEffect (GM.openInfoWindow created gmap marker)
    { emitter, listener } <- H.liftEffect HS.create
    sid <- H.subscribe emitter
    H.liftEffect $ listenToAll created listener Event events
    H.put
      $ state
          { infoWindow = Just opened
          , subscription = Just sid
          }
  Update { options, parent } -> do
    state <- H.get
    let
      update = flip GM.setOptions options
    infoWindow <- H.liftEffect (for state.infoWindow update)
    H.put
      $ state
          { infoWindow = infoWindow
          , options = options
          , parent = parent
          }
  Remove -> do
    state <- H.get
    closed <- H.liftEffect (for state.infoWindow GM.closeInfoWindow)
    for_ state.subscription H.unsubscribe
    H.liftEffect (for_ closed GM.deleteInfoWindow)
    H.put (state { infoWindow = Nothing })
  Event mvcEvent _ -> do
    H.raise (Message mvcEvent)

events :: Array GM.InfoWindowEvent
events =
  [ GM.CloseClick
  , GM.DomReady
  ]
