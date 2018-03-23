module UI.View.Component.Button where

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
import UI.Controller.Component.Button (eval,State,Action (..),initialState)

component :: forall i eff. Component Action State eff
component =
        {
          initialState : initialState "label"
        , view
        , eval
        }

view :: forall i w eff. (Action -> Eff (frp :: FRP | eff) Unit) -> State -> PrestoDOM Action w
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
                          , height $ V 30
                          , text state
                          , color "#ffffff"
                          , textSize "16"
                          , fontFamily "SourceSans Pro-SemiBold"
                          , gravity "center"
                          ]
                         
                      ]