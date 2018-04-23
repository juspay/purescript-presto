module Controller.FormField where

import Prelude

data Action = TextChanged String
type Label = String

type State =
  { text :: String
  , value :: String
  }

initialState :: Label -> State
initialState label = { text : label , value : "" }

eval :: Action -> State -> State
eval (TextChanged value) state = state { value = value }

