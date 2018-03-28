module Core where

import Prelude (Unit, bind, discard, (=<<), (>>=), (*>))

import Control.Monad.Aff (launchAff, makeAff, Canceler)
import Control.Monad.Aff.AVar (makeVar')
import Control.Monad.Eff (Eff)
import Data.Time.Duration (Milliseconds(..))
import Control.Monad.State.Trans as S
import Data.Function.Uncurried (runFn2)
import Data.StrMap (empty)
import Engineering.Helpers.Commons (callAPI', mkNativeRequest, showUI')
import Engineering.OS.Permission (checkIfPermissionsGranted, requestPermissions)
import Engineering.Types.App (AppEffects, CancelerEffects)
import Presto.Core.Flow (APIRunner, Flow, PermissionCheckRunner, PermissionRunner(PermissionRunner), PermissionTakeRunner,Runtime(Runtime), UIRunner, delay, run)
import UI.Flow as UI
import Remote.Flow as Remote
import Types.Remote

appFlow :: Flow Unit
appFlow = do
  UI.splash *> delay (Milliseconds 1000.0)
  operator     <- UI.chooseOperator =<< Remote.fetchOperators
  mobileNumber <- UI.askMobileNumber
  amount       <- UI.askAmount
  
  let billPayRequest = BillPayRequest
                        {mobileNumber:mobileNumber
                        ,amount: amount
                        ,operator:operator }

  Remote.payBill billPayRequest >>= UI.showStatus


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