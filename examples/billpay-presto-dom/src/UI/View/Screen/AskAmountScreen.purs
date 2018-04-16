module UI.View.Screen.AskAmountScreen where

import Prelude (Unit, ($), (>))
import Control.Monad.Eff (Eff)
import FRP (FRP)
import Data.Either(Either(..))
import Data.Number (fromString)
import Data.Maybe (fromMaybe)

import PrestoDOM.Elements.Elements (editText, imageView, linearLayout, textView)
import PrestoDOM.Properties (background, color, fontFamily, gravity, height, hint, imageUrl, margin, name, orientation, stroke, text, textSize, weight, width)
import PrestoDOM.Types.DomAttributes (Length(..))
import PrestoDOM.Events (onChange)
import PrestoDOM.Types.Core (PrestoDOM, Screen)
import PrestoDOM.Core (mapDom)

import UI.View.Component.Header as Header
import UI.View.Component.Button as Button
import UI.Types (Amount)

-- TODO: Take a Number from the Screen directly instead of String and converting it to a Number
data Action = AmountEntered String
            | BackFlow Header.Action
            | OnProceed Button.Action

type State = Amount

initialState :: State
initialState = 0.0

eval :: Action -> State -> Either Amount State
eval (AmountEntered amount) state = Right (fromMaybe 0.0 $ fromString amount)
eval (BackFlow action) state = Right state
eval (OnProceed action) state =
    if state > 0.0
        then Left state
        else Right state

askAmountScreen :: forall eff. Screen Action State eff Amount
askAmountScreen =
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
                          [ height $ V 0
                          , width Match_Parent
                          , stroke "1,#0A338D"
                          , margin "5,0,0,0"
                          ]
                          []
                      ]
                ],
              (mapDom Button.view push "PROCEED" OnProceed)
            ]
        ]
