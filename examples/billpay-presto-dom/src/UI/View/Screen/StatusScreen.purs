module UI.View.Screen.StatusScreen where

import Prelude (Unit, show, unit, ($), (<>))
import Control.Monad.Eff (Eff)
import FRP (FRP)
import Data.Either(Either(..))

import PrestoDOM.Elements.Elements (imageView, linearLayout, textView)
import PrestoDOM.Properties (background, color, fontFamily, gravity, height, imageUrl, margin, name, orientation, text, textSize, width)
import PrestoDOM.Types.DomAttributes (Length(..))
import PrestoDOM.Types.Core (PrestoDOM, Screen)


import UI.Types (MobileNumber , Amount , BillPayStatus)

data Action = Rendered

type State = {
    amount :: String
,   mobileNumber :: String
}

initialState :: String -> String -> State
initialState amount mobileNumber = {amount : amount, mobileNumber: mobileNumber}
  
eval :: Action -> State -> Either Unit State
eval Rendered state = Left unit

screen :: MobileNumber -> Amount -> BillPayStatus -> forall eff. Screen Action State eff Unit
screen mobileNumber amount billPayStatus =
  {
    initialState : initialState mobileNumber $ show amount
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