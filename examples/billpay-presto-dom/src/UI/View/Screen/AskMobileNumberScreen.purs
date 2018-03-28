module UI.View.Screen.AskMobileNumberScreen where

import Prelude (Unit, ($), (==))
import Control.Monad.Eff (Eff)
import FRP (FRP)
import Data.String (length)
import Data.Either(Either(..))

import PrestoDOM.Elements.Elements (editText, imageView, linearLayout, textView)
import PrestoDOM.Properties (background, color, fontFamily, gravity, height, hint, imageUrl, margin, name, orientation, stroke, text, textSize, weight, width)
import PrestoDOM.Types.DomAttributes (Length(..))
import PrestoDOM.Events (onChange)
import PrestoDOM.Types.Core (PrestoDOM, Screen)
import PrestoDOM.Core (mapDom)

import UI.View.Component.Header as Header
import UI.View.Component.Button as Button
import UI.Types (MobileNumber)

data Action = MobileNumberEntered MobileNumber
            | BackFlow Header.Action
            | OnProceed Button.Action

type State = MobileNumber

initialState :: State
initialState = ""
  
eval :: Action -> State -> Either MobileNumber State
eval (MobileNumberEntered mobileNumber) state = Right mobileNumber
eval (BackFlow action) state = Right state
eval (OnProceed action) state = 
    if (length state) == 10
        then Left state
        else Right state

screen :: forall eff. Screen Action State eff String
screen =
  {
    initialState
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
            ]
            [ (mapDom Header.view push "MobileNumber" BackFlow)
            , textView
                [ width $ V 150
                , height (V 25)
                , text "Enter Mobile Number"
                , color "#354520"
                , textSize "14"
                , fontFamily "SourceSans Pro-SemiBold"
                , margin "15,20,0,0"
                ],
              linearLayout
                [width Match_Parent
                ,height $ V 0
                ,weight "1"
                ,orientation "horizontal"
                ]
                [ imageView
                    [ height $ V 36
                    , width $ V 36
                    , imageUrl "a910"
                    , margin "15,10,0,0"
                    ],
                  linearLayout
                    [ height $ V 30
                    , width $  V 250
                    , orientation "vertical"
                    ]
                    [
                    editText
                      [ height (V 30)
                      , width $ V 250
                      , margin "5,10,0,0"
                      , textSize "18"
                      , name "name"
                      , color "#354520"
                      , hint "9705563642"
                      , stroke "0,#ffffff"
                      , onChange push MobileNumberEntered
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
