module Core where

import Prelude

import Control.Monad.Aff (launchAff, makeAff, Canceler)
import Control.Monad.Aff.AVar (makeVar')
import Control.Monad.Trans.Class
import Control.Monad.Eff (Eff)
import Data.Time.Duration
import Control.Monad.State.Trans as S
import Data.Function.Uncurried (runFn2)
import Data.StrMap (empty)
import Engineering.Helpers.Commons (callAPI', mkNativeRequest, showUI')
import Engineering.OS.Permission (checkIfPermissionsGranted, requestPermissions)
import Engineering.Types.App (AppEffects, CancelerEffects)
import Presto.Core.Flow (APIRunner, Flow, PermissionCheckRunner, PermissionRunner(PermissionRunner), PermissionTakeRunner, Runtime(Runtime), UIRunner, run, runScreen,forkScreen,delay)
import UI.View.Screen.SplashScreen (screen) as SplashScreen
import UI.View.Screen.ChooseOperatorScreen (screen) as ChooseOperator
import UI.View.Screen.AskMobileNumberScreen (screen) as AskMobileNumber
import UI.View.Screen.AskAmountScreen (screen) as AskAmount
import UI.View.Screen.StatusScreen (screen) as StatusScreen
import Remote.Flow as Remote

main :: Eff (AppEffects) (Canceler (CancelerEffects))
main = do
  let runtime = Runtime uiRunner permissionRunner apiRunner
  let freeFlow = S.evalStateT (run runtime appFlow)
  launchAff (makeVar' empty >>= freeFlow)
  where
    uiRunner :: UIRunner
    uiRunner a = makeAff (\err sc -> runFn2 showUI' sc a)

    permissionCheckRunner :: PermissionCheckRunner
    permissionCheckRunner = checkIfPermissionsGranted

    permissionTakeRunner :: PermissionTakeRunner
    permissionTakeRunner = requestPermissions

    permissionRunner :: PermissionRunner
    permissionRunner = PermissionRunner permissionCheckRunner permissionTakeRunner

    apiRunner :: APIRunner
    apiRunner request = makeAff (\err sc -> callAPI' err sc (mkNativeRequest request))

appFlow :: Flow Unit
appFlow = do
  _            <- forkScreen SplashScreen.screen
  _            <- delay (Milliseconds 1000.0)
  operators    <- Remote.fetchOperators
  operator     <- runScreen (ChooseOperator.screen operators)
  mobileNumber <- runScreen AskMobileNumber.screen
  amount       <- runScreen AskAmount.screen
  result       <- Remote.payBill mobileNumber amount operator
  _            <- runScreen (StatusScreen.screen mobileNumber amount result)
  pure unit