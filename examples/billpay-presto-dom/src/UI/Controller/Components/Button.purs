module UI.Controller.Component.Button where

import Prelude

data Action = ProceedClick
type Label = String

type State = String

initialState :: Label -> State
initialState label =  label 

eval :: Action -> State -> State
eval (ProceedClick) state = state 
