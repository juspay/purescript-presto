module UI.View.Screen.ChooseOperatorScreen where

import Prelude (Unit, map, ($))
import Control.Monad.Eff (Eff)
import FRP (FRP)
import Data.Either(Either(..))

import PrestoDOM.Elements.Elements (linearLayout, scrollView, textView)
import PrestoDOM.Properties (background, color, fontFamily, gravity, height, margin, name, orientation, text, textSize, weight, width)
import PrestoDOM.Types.DomAttributes (Length(..))
import PrestoDOM.Types.Core (PrestoDOM, Screen)
import PrestoDOM.Core (mapDom)
import UI.View.Component.Operator as Operator
import UI.Types (Operator)

data Action = OperatorSelected Operator.Action

type State = Array Operator

initialState :: State -> State
initialState operators = operators
  
eval :: Action -> State -> Either Operator State
eval (OperatorSelected (Operator.OperatorSelected operator)) state = Left operator


screen :: Array Operator -> forall eff. Screen Action State eff String
screen operators =
  {
    initialState : initialState operators
  , view
  , eval
  }

view :: forall w eff. (Action -> Eff (frp :: FRP | eff) Unit) -> State -> PrestoDOM Action w
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
