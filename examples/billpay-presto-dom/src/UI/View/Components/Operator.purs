module UI.View.Component.Operator where

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
import UI.Controller.Component.Operator (eval,State,Action (..),initialState)
import Data.String (toLower)

component :: forall i eff. Component Action State eff
component =
        {
          initialState : initialState "operator"
        , view
        , eval
        }

view :: forall i w eff. (Action -> Eff (frp :: FRP | eff) Unit) -> State -> PrestoDOM Action w
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