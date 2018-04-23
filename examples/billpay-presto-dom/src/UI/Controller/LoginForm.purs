module Controller.LoginForm where

import Prelude
import Data.Either(Either(..))

import Controller.FormField as FormField


data Action =
  Username FormField.Action
  | Password FormField.Action
  | SubmitClicked

type State =
  { errorMessage :: String
  , usernameState :: FormField.State
  , passwordState :: FormField.State
  }

initialState :: State
initialState =
  { errorMessage : ""
  , usernameState : (FormField.initialState "username")
  , passwordState : (FormField.initialState "password")
  }

eval :: Action -> State -> Either Unit State
eval (Username action) state = Right $ state { usernameState = FormField.eval action state.usernameState }
eval (Password action) state = Right $ state { passwordState = FormField.eval action state.passwordState }
eval SubmitClicked state =
    if state.passwordState.value == "blueberry" && state.usernameState.value /= ""
        then (Left unit)
        else (Right $ state { errorMessage = "Your account is blocked" })
