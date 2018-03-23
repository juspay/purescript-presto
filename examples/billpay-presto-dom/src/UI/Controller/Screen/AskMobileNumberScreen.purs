module UI.Controller.Screen.AskMobileNumberScreen where

import Prelude
import Data.String
import Data.Either(Either(..))

import UI.Types(MobileNumber)
import UI.Controller.Component.Button as ButtonController
import UI.Controller.Component.Header as HeaderController

data Action = MobileNumberEntered MobileNumber
            | BackFlow HeaderController.Action
            | OnProceed ButtonController.Action

type State = MobileNumber

initialState :: State
initialState = ""
  
eval :: Action -> State -> Either MobileNumber State
eval (MobileNumberEntered mobileNumber) state = Right mobileNumber
eval (BackFlow action) state = Right state
eval (OnProceed action) state = 
    if (length state) == 10
        then Left state
        else Right state