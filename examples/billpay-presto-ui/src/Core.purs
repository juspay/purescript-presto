module Core where

import Prelude

import Effect.Aff (launchAff_, makeAff, nonCanceler)
import Effect.Aff.AVar (new)
import Effect (Effect)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.State.Trans as S
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn2)
import Foreign.Object (empty)
import Engineering.Helpers.Commons (callAPI', mkNativeRequest, showUI')
import Engineering.OS.Permission (checkIfPermissionsGranted, requestPermissions)
import Presto.Core.Flow (APIRunner, Flow, PermissionCheckRunner, PermissionRunner(..), PermissionTakeRunner, Runtime(..), UIRunner, run, forkUI)
import Product.BillPay (billPayFlow)
import UI.Types (InitScreen(..))

main :: Effect Unit
main = do
  let runtime = Runtime uiRunner permissionRunner apiRunner
  let freeFlow = S.evalStateT (run runtime appFlow)
  launchAff_ (new empty >>= freeFlow)
  where
    uiRunner :: UIRunner
    uiRunner a = makeAff (\callback -> runFn2 showUI' (Right >>> callback) a *> pure nonCanceler)

    permissionCheckRunner :: PermissionCheckRunner
    permissionCheckRunner = checkIfPermissionsGranted

    permissionTakeRunner :: PermissionTakeRunner
    permissionTakeRunner = requestPermissions

    permissionRunner :: PermissionRunner
    permissionRunner = PermissionRunner permissionCheckRunner permissionTakeRunner

    apiRunner :: APIRunner
    apiRunner request = makeAff (\callback ->
      callAPI' (Left >>> callback) (Right >>> callback) (mkNativeRequest request) *> pure nonCanceler)

mainFlow :: Flow Unit
mainFlow = do
  result <- (runExceptT $ billPayFlow)
  case result of
    Right a -> pure unit
    Left a -> mainFlow

initializeUI :: Flow Unit
initializeUI = forkUI InitScreen *> pure unit

appFlow :: Flow Unit
appFlow = do
  initializeUI
  mainFlow
