module Controller.LoginForm where

import Prelude

import Controller.FormField as FormField
import PrestoDOM (exit, updateAndExit, continue, Eval)
import PrestoDOM.Types.DomAttributes (Visibility(..))

data Action =
  UsernameAction FormField.Action
  | PasswordAction FormField.Action
  | SubmitClicked

type State =
  { errorMessage :: String
  , usernameState :: FormField.State
  , passwordState :: FormField.State
  , visibility :: Visibility
  }

initialState :: State
initialState =
  { errorMessage : ""
  , usernameState : (FormField.initialState "username")
  , passwordState : (FormField.initialState "password")
  , visibility : VISIBLE
  }

eval :: forall eff. Action -> State -> Eval eff Action Unit State
eval (UsernameAction action) state = continue $ state { usernameState = FormField.eval action state.usernameState }
eval (PasswordAction action) state = continue $ state { passwordState = FormField.eval action state.passwordState }
eval SubmitClicked state =
    if state.passwordState.value == "blueberry" && state.usernameState.value /= ""
        then (updateAndExit (state { visibility = GONE }) unit)
        else (continue $ state { errorMessage = "Your account is blocked" })
