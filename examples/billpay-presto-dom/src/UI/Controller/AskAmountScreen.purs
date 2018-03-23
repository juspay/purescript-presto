module Controller.AskAmountScreen where

import Prelude
import Data.Either(Either(..))

import Types.UI(Amount)

data Action = AmountEntered Amount

type State = Amount

initialState :: State
initialState = 1.0
  
eval :: Action -> State -> Either Amount State
eval (AmountEntered amount) state = do
    if amount > 0.0
        then (Left 1.0)
        else (Right state)
