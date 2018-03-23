module View.ChooseOperatorScreen where

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
import Controller.ChooseOperatorScreen(Action(..), State, eval, initialState)
import Types.UI (Operator)

screen :: Array Operator -> forall eff. Screen Action State eff String
screen operators =
  {
    initialState : initialState operators
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
