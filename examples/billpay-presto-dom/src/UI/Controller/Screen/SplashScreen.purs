module UI.Controller.Screen.SplashScreen where

import Prelude
import Data.Either(Either(..))


data Action = Rendered

type State = {}

initialState :: State
initialState = {}
  
eval :: Action -> State -> Either Unit State
eval Rendered state = Left unit
