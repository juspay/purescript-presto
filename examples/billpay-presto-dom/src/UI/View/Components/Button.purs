module UI.View.Component.Button where

import Prelude (Unit, const, ($))
import Control.Monad.Eff (Eff)
import FRP (FRP)

import PrestoDOM.Elements.Elements (linearLayout, textView)
import PrestoDOM.Properties (background, color, cornerRadius, fontFamily, gravity, height, margin, name, text, textSize, width)
import PrestoDOM.Types.DomAttributes (Length(..))
import PrestoDOM.Events (onClick)
import PrestoDOM.Types.Core (PrestoDOM)

data Action = ProceedClick

type State = String

initialState :: String -> State
initialState label =  label

eval :: Action -> State -> State
eval (ProceedClick) state = state

view :: forall w eff. (Action -> Eff (frp :: FRP | eff) Unit) -> State -> PrestoDOM Action w
view push state =  linearLayout
                      [ height $ V 50
                      , width Match_Parent
                      , background "#212790"
                      , gravity "center"
                      , name "name"
                      , margin "20,0,20,20"
                      , cornerRadius "5"
                      , onClick push (const ProceedClick)
                      ]
                      [ textView
                          [ width $ V 200
                          , height $ V 16
                          , text state
                          , color "#ffffff"
                          , textSize "16"
                          , fontFamily "SourceSans Pro-SemiBold"
                          , gravity "center"
                          ]

                      ]