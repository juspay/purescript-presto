module UI.View.Screen.SplashScreen where

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
import UI.Controller.Screen.SplashScreen(Action(..), State, eval, initialState)

screen :: forall eff. Screen Action State eff Unit
screen =
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
        , background "#f8f8f8"
        , gravity "center"
        , name "rootNode"
        ]
        [ 
        linearLayout
          [ height $ V 610
          , width $ V 360
          , background "#ffffff"
          , orientation "vertical"
          , gravity "center"
          ]
          [ textView
              [ width Match_Parent
              , height (V 25)
              , text "Bill Pay"
              , color "#1991eb"
              , textSize "32"
              , fontFamily "SourceSans Pro-SemiBold"
              , gravity "center"
              ],
            textView
              [ width Match_Parent
              , height (V 25)
              , text "Make recharge simple"
              , color "#1991eb"
              , textSize "14"
              , fontFamily "SourceSans Pro-Regular"
              , margin "0,20,0,0"
              , gravity "center"
              ]
          ]
        ]
