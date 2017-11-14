module Core where

import Prelude

import Control.Monad.Aff (launchAff, makeAff,Canceler)
import Control.Monad.Aff.AVar (makeVar')
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.State.Trans as S
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn2)
import Data.StrMap (empty)
import Engineering.Helpers.Commons (callAPI', mkNativeRequest, showUI')
import Engineering.OS.Permission (checkIfPermissionsGranted, requestPermissions)
import Engineering.Types.App (AppEffects,CancelerEffects)
import Presto.Core.Flow (APIRunner, Flow, PermissionCheckRunner, PermissionRunner(..), PermissionTakeRunner, Runtime(..), UIRunner, run)
import Product.BillPay (billPayFlow)

launchApp :: Eff (AppEffects) (Canceler (CancelerEffects))
launchApp = do
  let runtime = Runtime uiRunner permissionRunner apiRunner
  let freeFlow = S.evalStateT (run runtime mainFlow)
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

mainFlow :: Flow Unit
mainFlow = do
  result <- (runExceptT $ billPayFlow)
  case result of
    Right a -> pure unit
    Left a -> mainFlow