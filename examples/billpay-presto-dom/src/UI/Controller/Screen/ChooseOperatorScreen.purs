module UI.Controller.Screen.ChooseOperatorScreen where

import Prelude
import Data.Either(Either(..))

import UI.Types(Operator)
import UI.Controller.Component.Operator as ControllerOperator


data Action = OperatorSelected ControllerOperator.Action

type State = Array Operator

initialState :: State -> State
initialState operators = operators
  
eval :: Action -> State -> Either Operator State
eval (OperatorSelected (ControllerOperator.OperatorSelected operator)) state = Left operator
