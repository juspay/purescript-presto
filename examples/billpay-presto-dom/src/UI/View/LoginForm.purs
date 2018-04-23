module View.LoginForm where

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
import Controller.LoginForm(Action(..), State, eval, initialState)
import View.FormField as FormField

screen :: forall eff. Screen Action State eff Unit
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
    , background "#323232"
    , gravity "center"
    , name "rootNode"
    ]
    [ linearLayout
      [ height $ V 600
      , width $ V 400
      , background "#000000"
      , orientation "vertical"
      , gravity "center"
      ]
      [ (mapDom FormField.view push state.usernameState Username)
      , (mapDom FormField.view push state.passwordState Password)
      , linearLayout
        [ height $ V 150
        , width Match_Parent
        , orientation "vertical"
        , margin "20,20,20,20"
        , gravity "center"
        ]
        [ linearLayout
          [ height $ V 50
          , width Match_Parent
          , margin "20,0,20,0"
          , text state.errorMessage
          ]
          []
        , linearLayout
          [ height $ V 50
          , width Match_Parent
          , margin "20,70,20,20"
          , background "#969696"
          , gravity "center"
          , visibility "not"
          , name "name"
          , onClick push (const SubmitClicked)
          ]
          [
            textView
            [ width (V 80)
            , height (V 25)
            , text "Submit"
            , textSize "28"
            , name "name"
            ]
          ]
        ]
      ]
    ]
