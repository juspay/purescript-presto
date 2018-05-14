module View.LoginForm where

import Prelude

import PrestoDOM.Elements.Elements
import PrestoDOM.Properties
import PrestoDOM.Types.DomAttributes
import PrestoDOM.Events (onChange, onClick)
import PrestoDOM.Types.Core (PrestoDOM, Screen, PropEff)
import PrestoDOM.Core (mapDom)
import Controller.LoginForm (Action(..), State, eval, initialState)
import View.FormField as FormField

screen :: forall eff. Screen Action State eff Unit
screen =
  { initialState
  , view
  , eval
  }

view :: forall w eff. (Action -> PropEff eff) -> State -> PrestoDOM (PropEff eff) w
view push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background "#323232"
    , gravity CENTER
    ]
    [ linearLayout
      [ height $ V 600
      , width $ V 400
      , background "#000000"
      , orientation VERTICAL
      , visibility state.visibility
      , gravity CENTER
      ]
      [ (mapDom FormField.view push state.usernameState UsernameAction [])
      , (mapDom FormField.view push state.passwordState PasswordAction [])
      , linearLayout
        [ height $ V 150
        , width MATCH_PARENT
        , orientation VERTICAL
        , margin $ Margin 20 20 20 20
        , gravity CENTER
        ]
        [ textView
          [ height $ V 50
          , width MATCH_PARENT
          , margin $ MarginHorizontal 20 20
          , text state.errorMessage
          ]
        , linearLayout
          [ height $ V 50
          , width MATCH_PARENT
          , margin $ Margin 20 20 20 20
          , background "#969696"
          , gravity CENTER
          , visibility VISIBLE
          , onClick push (const SubmitClicked)
          ]
          [
            textView
            [ width (V 80)
            , height (V 25)
            , text "Submit"
            , textSize 28
            ]
          ]
        ]
      ]
    ]
