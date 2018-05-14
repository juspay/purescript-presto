module Core where

import Prelude

import Control.Monad.Aff (launchAff_, makeAff, nonCanceler)
import Control.Monad.Aff.AVar (makeVar)
import Control.Monad.Eff (Eff)
import Control.Monad.State.Trans as S
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn2)
import Data.StrMap (empty)
import Engineering.Helpers.Commons (callAPI', mkNativeRequest, showUI')
import Engineering.OS.Permission (checkIfPermissionsGranted, requestPermissions)
import Engineering.Types.App (AppEffects)
import Presto.Core.Flow (APIRunner, Flow, PermissionCheckRunner, PermissionRunner(..), PermissionTakeRunner, Runtime(..), UIRunner, run, initUI, runScreen)
import View.LoginForm (screen) as LoginForm

main :: Eff (AppEffects) Unit
main = do
  let runtime = Runtime uiRunner permissionRunner apiRunner
  let freeFlow = S.evalStateT (run runtime appFlow)
  launchAff_ (makeVar empty >>= freeFlow)
  where
    uiRunner :: UIRunner
    uiRunner a = makeAff (\cb -> runFn2 showUI' (cb <<< Right) a *> pure nonCanceler)

    permissionCheckRunner :: PermissionCheckRunner
    permissionCheckRunner = checkIfPermissionsGranted

    permissionTakeRunner :: PermissionTakeRunner
    permissionTakeRunner = requestPermissions

    permissionRunner :: PermissionRunner
    permissionRunner = PermissionRunner permissionCheckRunner permissionTakeRunner

    apiRunner :: APIRunner
    apiRunner request = makeAff (\cb ->
                          callAPI' (cb <<< Left) (cb <<< Right) (mkNativeRequest request) *> pure nonCanceler)

appFlow :: Flow Unit
appFlow = do
  _ <- initUI
  result <- runScreen LoginForm.screen
  pure unit