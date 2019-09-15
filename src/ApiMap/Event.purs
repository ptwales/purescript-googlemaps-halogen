module GMaps.ApiMap.Event where

import Effect.Aff.Class (class MonadAff)
import GMaps.MVC.MVCEvent (class MVCEvent) as GM
import GMaps.MVC.MVCObject (class MVCObject, addListener, removeListener) as GM
import Halogen.Query.EventSource (EventSource)
import Halogen.Query.EventSource as ES
import Prelude (bind, pure, (<<<))

eventSource ::
  forall m object event d action.
  MonadAff m =>
  GM.MVCEvent event =>
  GM.MVCObject object event =>
  object -> (event -> d -> action) -> event -> EventSource m action
eventSource object f event =
  ES.effectEventSource \emitter -> do
    let
      handler = ES.emit emitter <<< f event
    listener <- GM.addListener object event handler
    pure (ES.Finalizer (GM.removeListener listener))
