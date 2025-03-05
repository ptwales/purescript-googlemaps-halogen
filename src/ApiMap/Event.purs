module GMaps.ApiMap.Event where

import Data.Traversable (traverse_)
import Effect (Effect)
import GMaps.MVC.MVCEvent (class MVCEvent) as GM
import GMaps.MVC.MVCObject (class MVCObject, addListener) as GM
import Halogen.Subscription as HS
import Prelude (Unit, void, (<<<))

listenToAll
  :: forall object event d action
   . GM.MVCEvent event
  => GM.MVCObject object event
  => object
  -> HS.Listener action
  -> (event -> d -> action)
  -> Array event
  -> Effect Unit
listenToAll object listener act = traverse_ (listenTo object listener act)

listenTo
  :: forall object event d action
   . GM.MVCEvent event
  => GM.MVCObject object event
  => object
  -> HS.Listener action
  -> (event -> d -> action)
  -> event
  -> Effect Unit
listenTo object listener act event =
  let
    notify = HS.notify listener <<< act event
    listen = GM.addListener object event notify
  in
    void listen
