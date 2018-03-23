module UI.Controller.Component.Operator where

import Prelude

data Action = OperatorSelected String

type State = String

initialState :: String -> State
initialState op = op

eval :: Action -> State -> State
eval (OperatorSelected operator) state = state 

