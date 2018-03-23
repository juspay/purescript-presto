module UI.View.Screen.ChooseOperatorScreen where

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
import UI.Controller.Screen.ChooseOperatorScreen(Action(..), State, eval, initialState)
import UI.Types (Operator)
import UI.View.Component.Operator as Operator

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
        , background "#f8f8f8"
        , gravity "center"
        , name "rootNode"
        ]
        [ linearLayout
            [ height $ V 610
            , width $ V 360
            , background "#ffffff"
            , orientation "vertical"
            , weight "0"
            ]
            [ textView
                [ width Match_Parent
                , height (V 40)
                , text "Choose Operator"
                , color "#0A338D"
                , textSize "24"
                , fontFamily "SourceSans Pro-SemiBold"
                , gravity "left"
                , margin "20,10,0,0"
                ],
              scrollView
                [ height Match_Parent
                , width Match_Parent
                ]
                [ linearLayout
                    [ height Match_Parent
                    , width Match_Parent
                    , orientation "vertical"
                    ]
                    (map (\internalState -> mapDom Operator.view push internalState OperatorSelected) state)
                ]  
            ]
        ]
