module Controller.AskAmountScreen where

import Prelude
import Data.Either(Either(..))

import Controller.FormField as FormField


data Action = AmountEntered String

type State = String

initialState :: State
initialState = "hi"
  
eval :: Action -> State -> Either Unit State
eval (AmountEntered amount) state = do
    if amount > "0"
        then (Left unit)
        else (Right state)
