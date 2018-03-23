module Controller.SplashScreen where

import Prelude
import Data.Either(Either(..))


data Action = Rendered

type State = String

initialState :: State
initialState = "hi"
  
eval :: Action -> State -> Either Unit State
eval Rendered state = Left unit
