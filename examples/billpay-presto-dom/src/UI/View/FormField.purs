module View.FormField where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)
import FRP (FRP)

import PrestoDOM.Elements.Elements
import PrestoDOM.Properties
import PrestoDOM.Types.DomAttributes
import PrestoDOM.Events (onChange)
import PrestoDOM.Types.Core (Component, PrestoDOM)
import Controller.FormField(Action(..), State, eval, initialState)

component :: forall i eff. Component Action State eff
component =
  {
    initialState : initialState "Label"
  , view
  , eval
  }

view :: forall i w eff. (Action -> Eff (frp :: FRP | eff) Unit) -> State -> PrestoDOM Action w
view push state =
  linearLayout
    [ height $ V 150
    , width Match_Parent
    , orientation "vertical"
    , margin "20,20,20,20"
    ]
    [ linearLayout
        [ height $ V 30
        , width Match_Parent
        , margin "10,20,20,20"
        , text state.text
        , textSize "28"
        ]
        []
    , linearLayout
        []
        [ editText
        [ height (V 40)
        , width Match_Parent
        , margin "10,10,10,10"
        , textSize "20"
        , name "name"
        , color "#00000"
        , text state.value
        , onChange push TextChanged
        ]
        ]
    ]
