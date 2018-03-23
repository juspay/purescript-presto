module UI.View.Component.Header where

import Prelude
import Control.Monad.Eff (Eff)
import DOM (DOM)
import FRP (FRP)
import Data.Either(Either(..))
import PrestoDOM.Elements.Elements
import PrestoDOM.Properties
import PrestoDOM.Types.DomAttributes
import PrestoDOM.Events (onChange, onClick)
import PrestoDOM.Types.Core (Component, PrestoDOM, Screen)
import PrestoDOM.Core (mapDom)
import UI.Controller.Component.Header (eval,State,Action (..),initialState)

component :: forall i eff. Component Action State eff
component =
        {
          initialState : initialState "Label"
        , view
        , eval
        }

view :: forall i w eff. (Action -> Eff (frp :: FRP | eff) Unit) -> State -> PrestoDOM Action w
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
