module UI.Controller.Screen.AskAmountScreen where

import Prelude
import Data.Either(Either(..))
import Data.Number
import Data.Maybe

import UI.Types(Amount)
import UI.Controller.Component.Button as ButtonController
import UI.Controller.Component.Header as HeaderController

data Action = AmountEntered String
            | BackFlow HeaderController.Action
            | OnProceed ButtonController.Action

type State = Amount

initialState :: State
initialState = 0.0
  
eval :: Action -> State -> Either Amount State
eval (AmountEntered amount) state = Right (fromMaybe 0.0 $ fromString amount)
eval (BackFlow action) state = Right state
eval (OnProceed action) state = 
    if state > 0.0 
        then Left state  
        else Right state