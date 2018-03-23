module UI.View.Screen.AskAmountScreen where

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
import UI.Controller.Screen.AskAmountScreen(Action(..), State, eval, initialState)
import UI.Types
import UI.View.Component.Header as Header
import UI.View.Component.Button as Button


screen :: forall eff. Screen Action State eff Amount
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
        [ linearLayout
            [ height $ V 610
            , width $ V 360
            , background "#ffffff"
            , orientation "vertical"
            ]
            [ (mapDom Header.view push "Amount" BackFlow),
              textView
                [ width  Match_Parent
                , height (V 25)
                , text "Enter Recharge Amount"
                , color "#354520"
                , textSize "14"
                , fontFamily "SourceSans Pro-SemiBold"
                , margin "15,20,0,0"
                ],
              linearLayout
                [ width Match_Parent
                , height $ V 0
                , weight "1"
                , orientation "horizontal"
                ]
                [ imageView
                    [ height $ V 36
                    , width $ V 36
                    , imageUrl "rupee"
                    , margin "15,10,0,0"
                    ],
                    linearLayout
                      [ height $ V 30
                      , width $  V 250
                      , orientation "vertical"
                      ]
                      [ editText
                          [ height (V 30)
                          , width $ V 250
                          , margin "5,10,0,0"
                          , textSize "18"
                          , name "name"
                          , color "#354520"
                          , hint "349"
                          , stroke "0,#ffffff"
                          , onChange push AmountEntered
                          ],
                        linearLayout
                          [ height $ V 1
                          , width Match_Parent
                          , stroke "1,#0A338D"
                          , margin "5,0,0,0"
                          ]
                          []
                      ]  
                ],
              (mapDom Button.view push "Proceed" OnProceed)  
            ]
        ] 
