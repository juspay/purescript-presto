module UI.Controller.Component.Header where

import Prelude

data Action = Backclick

type State = String

initialState :: String -> State
initialState label = label

eval :: Action -> State -> State
eval (Backclick) state = state 

