module UI.Flow where

import Prelude (Unit)
import Types.Storage (Transaction)
import UI.View.Screen.SplashScreen (splashScreen)
import UI.View.Screen.ChooseOperatorScreen (screen) as ChooseOperator
import UI.View.Screen.AskMobileNumberScreen (screen) as AskMobileNumber
import UI.View.Screen.AskAmountScreen (askAmountScreen)
import UI.View.Screen.StatusScreen (screen) as StatusScreen
import Presto.Core.Flow(runScreen, forkScreen, Flow)
import UI.Types (Amount, MobileNumber, Operator)

splash :: Flow Unit
splash = forkScreen splashScreen

chooseOperator :: Array String -> Flow Operator
chooseOperator operators = runScreen (ChooseOperator.screen operators)

askMobileNumber :: Flow MobileNumber
askMobileNumber = runScreen AskMobileNumber.screen

askAmount:: Flow Amount
askAmount = runScreen askAmountScreen

showStatus :: Transaction -> Flow Unit
showStatus txn =  runScreen (StatusScreen.screen txn)