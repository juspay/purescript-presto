module Controller.AskMobileNumberScreen where

import Prelude
import Data.Either(Either(..))

import Types.UI(MobileNumber)

data Action = MobileNumberEntered MobileNumber

type State = MobileNumber

initialState :: State
initialState = "hi"
  
eval :: Action -> State -> Either MobileNumber State
eval (MobileNumberEntered mobileNumber) state = do
    if mobileNumber > "0"
        then (Left mobileNumber)
        else (Right state)
