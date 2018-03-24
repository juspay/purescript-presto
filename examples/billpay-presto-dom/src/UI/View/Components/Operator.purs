module UI.View.Component.Operator where

import Prelude (Unit, const, ($))
import FRP (FRP)
import Control.Monad.Eff (Eff)
import Data.String (toLower)

import PrestoDOM.Elements.Elements (imageView, linearLayout, textView)
import PrestoDOM.Properties (color, fontFamily, gravity, height, imageUrl, margin, name, orientation, text, textSize, width)
import PrestoDOM.Types.DomAttributes (Length(..))
import PrestoDOM.Events (onClick)
import PrestoDOM.Types.Core (Component, PrestoDOM)


data Action = OperatorSelected String

type State = String

initialState :: String -> State
initialState op = op

eval :: Action -> State -> State
eval (OperatorSelected operator) state = state 

component :: forall eff. Component Action State eff
component =
        {
          initialState : initialState "operator"
        , view
        , eval
        }

view :: forall w eff. (Action -> Eff (frp :: FRP | eff) Unit) -> State -> PrestoDOM Action w
view push state = linearLayout
                    [ height $ V 50
                    , width Match_Parent
                    , orientation "horizontal"
                    , gravity "center_vertical"
                    , name "btn"
                    , onClick push (const $ OperatorSelected state)
                    , margin "20,20,0,0"
                    ]
                    [ imageView
                        [ height $ V 30
                        , width $ V 30
                        , imageUrl $ toLower state
                        
                        ],
                      textView
                        [ width Match_Parent
                        , height $ V 30
                        , text state
                        , color "#000000"
                        , textSize "24"
                        , fontFamily "SourceSans Pro-SemiBold"
                        , margin "20,0,0,0"
                        ]
                    ]