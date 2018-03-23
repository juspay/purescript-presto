module Controller.ChooseOperatorScreen where

import Prelude
import Data.Either(Either(..))

import Types.UI(Operator)


data Action = OperatorSelected Operator

type State = Array Operator

initialState :: State -> State
initialState operators = operators
  
eval :: Action -> State -> Either Operator State
eval (OperatorSelected operator) state = 
    Left operator
