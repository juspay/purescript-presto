module UI.View.Component.Header where

import Prelude (Unit, const, ($))
import Control.Monad.Eff (Eff)
import FRP (FRP)

import PrestoDOM.Elements.Elements (imageView, linearLayout, textView)
import PrestoDOM.Properties (color, fontFamily, gravity, height, imageUrl, margin, name, orientation, text, textSize, width)
import PrestoDOM.Types.DomAttributes (Length(..))
import PrestoDOM.Events (onClick)
import PrestoDOM.Types.Core (PrestoDOM)

data Action = Backclick

type State = String

initialState :: String -> State
initialState label = label

eval :: Action -> State -> State
eval (Backclick) state = state

view :: forall w eff. (Action -> Eff (frp :: FRP | eff) Unit) -> State -> PrestoDOM Action w
view push state = linearLayout
                    [ height $ V 50
                    , width Match_Parent
                    , orientation "horizontal"
                    , gravity "center_vertical"
                    ]
                    [ imageView
                        [ height $ V 54
                        , width $ V 54
                        , imageUrl "icon1"
                        , name "btn"
                        , onClick push (const Backclick)
                        ],
                      textView
                        [ width $ V 200
                        , height $ V 30
                        , text state
                        , color "#0A338D"
                        , textSize "24"
                        , fontFamily "SourceSans Pro-Regular"
                        , margin "4,0,0,0"
                        ]
                    ]
