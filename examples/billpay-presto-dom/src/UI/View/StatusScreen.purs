module View.StatusScreen where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)
import FRP (FRP)

import PrestoDOM.Elements.Elements
import PrestoDOM.Properties
import PrestoDOM.Types.DomAttributes
import PrestoDOM.Events (onChange, onClick)
import PrestoDOM.Types.Core (Component, PrestoDOM, Screen)
import PrestoDOM.Core (mapDom)
import Controller.StatusScreen(Action(..), State, eval, initialState)
import Types.UI (MobileNumber , Amount , BillPayStatus)

screen :: MobileNumber -> Amount -> BillPayStatus -> forall eff. Screen Action State eff Unit
screen mobileNumber amount billPayStatus =
  {
    initialState
  , view
  , eval
  }

view :: forall i w eff. (Action -> Eff (frp :: FRP | eff) Unit) -> State -> PrestoDOM Action w
view push state = 
    linearLayout
        [ height Match_Parent
        , width Match_Parent
        , background "#323232"
        , gravity "center"
        , name "rootNode"
        ][]
