module UI.Controller.Screen.StatusScreen where

import Prelude
import Data.Either(Either(..))

data Action = Rendered

type State = {
    amount :: String
,   mobileNumber :: String
}

initialState :: String -> String -> State
initialState amount mobileNumber = {amount : amount, mobileNumber: mobileNumber}
  
eval :: Action -> State -> Either Unit State
eval Rendered state = Left unit
