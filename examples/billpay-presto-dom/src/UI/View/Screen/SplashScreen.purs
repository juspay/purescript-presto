module UI.View.Screen.SplashScreen where

import Prelude (Unit, unit, ($))
import Control.Monad.Eff (Eff)
import FRP (FRP)
import Data.Either(Either(..))

import PrestoDOM.Elements.Elements (linearLayout, textView)
import PrestoDOM.Properties (background, color, fontFamily, gravity, height, margin, name, orientation, text, textSize, width)
import PrestoDOM.Types.DomAttributes (Length(..))
import PrestoDOM.Types.Core (PrestoDOM, Screen)

data Action = Rendered

type State = {}

initialState :: State
initialState = {}
  
eval :: Action -> State -> Either Unit State
eval Rendered state = Left unit

screen :: forall eff. Screen Action State eff Unit
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
        [ 
        linearLayout
          [ height $ V 610
          , width $ V 360
          , background "#ffffff"
          , orientation "vertical"
          , gravity "center"
          ]
          [ textView
              [ width Match_Parent
              , height (V 25)
              , text "Bill Pay"
              , color "#1991eb"
              , textSize "32"
              , fontFamily "SourceSans Pro-SemiBold"
              , gravity "center"
              ],
            textView
              [ width Match_Parent
              , height (V 25)
              , text "Make recharge simple"
              , color "#1991eb"
              , textSize "14"
              , fontFamily "SourceSans Pro-Regular"
              , margin "0,20,0,0"
              , gravity "center"
              ]
          ]
        ]
