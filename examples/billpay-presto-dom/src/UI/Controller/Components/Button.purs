module UI.Controller.Component.Button where

import Prelude

data Action = ProceedClick

type State = String

initialState :: String -> State
initialState label =  label 

eval :: Action -> State -> State
eval (ProceedClick) state = state 
