# PureScript Google Maps - Halogen

These are the halogen components corresponding to the [purescript-googlemaps][ffi] which provides the FFI bindings to google maps.

Since this project depends on [purescript-googlemaps][ffi], the googleapis maps javascript loaded as required by [purescript-googlemaps][ffi].

## Usage

All interaction with the google map happen through the `ApiMap` component.

- All settings of the map are controlled by inputs of the `ApiMap` component.
- All child components are controlled by adding their settings and a reference key to the relevant input field of the `ApiMap` component.
- Events such as mouse clicks or keyboard presses are raised as output of the `ApiMap` component.
  + Events of child components are wrapped in a data type with the child's key.
- Some queries can be made to the map just as any halogen component but most information about the map should be readily avialable by checking the input provided to the map.
  + Queries to child components can be made by wrapping them in data types with the children's key.

## Example

```purs
import Prelude
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import GMaps.ApiMap as ApiMap
import GMaps.ApiMap.Marker as Marker
import GMaps.Map (defMapOptions) as GM
import GMaps.Marker (defMaperOptions) as GM
import GMaps.MVC.MouseEvent (MVCMouseEvent(..)) as GM
import GMaps.Marker.MarkerEvent (MarkerEvent(..)) as GM

data Action = ApiMap GM.Output

type State = {}

type ChildSlots = ( googleMap :: ApiMap.Slot Unit )

_googleMap :: SProxy "googleMap"
_googleMap = SProxy

renderMap :: forall m. H.ComponentHTML Action ChildSlots m
renderMap =
  let center = { lat: 37.4228934, lng: (-122.0848761) }
      input =
        { mapOptions = (GM.defMapOptions center) { zoom = 12 }
        , markers =
          [ { key: 1
            , options: GM.defMarkerOptions center
            , infoWindow: Nothing
          } ]
        }
      listen = Just <<< GMapEvent
  in HH.slot _googleMap unit ApiMap.ui input listen

handleAction :: forall m. Action -> H.HalogenM State Action ChildSlots Output m Unit
handleAction = case _ of
  GMapEvent output -> case output of
    ApiMap.MarkerMessage key (Marker.Message (GM.MarkerEvent GM.Click mouseEvent -> do
     -- ...
     pure unit

```
[ffi]: https://github.com/ptwales/purescript-googlemaps
