module UI.View.Screen.StatusScreen where

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
import UI.Controller.Screen.StatusScreen(Action(..), State, eval, initialState)
import UI.Types (MobileNumber , Amount , BillPayStatus)

screen :: MobileNumber -> Amount -> BillPayStatus -> forall eff. Screen Action State eff Unit
screen mobileNumber amount billPayStatus =
  {
    initialState : initialState mobileNumber $ show amount
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
            , gravity "center_vertical"  
            ]
            [ imageView
                [ height $ V 95 
                , width $ V 112
                , imageUrl "status"
                , gravity "left"
                , margin "20,0,0,0"
                ],
              textView
                [ width Match_Parent
                , height (V 25)
                , text "Suceess"
                , color "#484848"
                , textSize "32"
                , fontFamily "SourceSans Pro-Bold"
                , margin "20,20,0,0"
                ],
              textView
                [ width Match_Parent
                , height (V 25)
                , text $ "You have successfully recharged " <> state.mobileNumber <> " with amount Rs " <> state.amount
                , color "#354520"
                , textSize "16"
                , fontFamily "SourceSans Pro-Regular"
                , margin "20,20,0,0"
                ]
            ]
        ]