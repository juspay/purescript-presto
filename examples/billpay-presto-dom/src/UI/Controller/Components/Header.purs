module UI.Controller.Component.Header where

import Prelude

data Action = Backclick
type Label = String

type State = String

initialState :: Label -> State
initialState label = label

eval :: Action -> State -> State
eval (Backclick) state = state 

