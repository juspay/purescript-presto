module Core where

import Prelude

import Control.Monad.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Control.Monad.Aff.AVar (makeVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.State.Trans as S
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn2)
import Data.StrMap (empty)
import Engineering.Helpers.Commons (callAPI', mkNativeRequest, showUI')
import Engineering.Types.App (AppEffects)
import Presto.Core.Flow (APIRunner, Flow, PermissionRunner(..), Runtime(..), UIRunner, run)
import Presto.Core.Types.App (STORAGE)
import Presto.Core.Types.Permission (Permission, PermissionResponse, PermissionStatus(..))
import Product.BillPay (billPayFlow)

launchApp :: Eff (AppEffects) Unit
launchApp = do
  let runtime = Runtime uiRunner permissionRunner apiRunner
  let freeFlow = S.evalStateT (run runtime mainFlow)
  launchAff_ (makeVar empty >>= freeFlow)
  where
    uiRunner :: UIRunner
    uiRunner a = makeAff (\callback -> runFn2 showUI' (Right >>> callback) a *> pure nonCanceler)

    permissionRunner :: PermissionRunner
    permissionRunner = PermissionRunner defaultPermissionStatus defaultPermissionRequest

    apiRunner :: APIRunner
    apiRunner request = makeAff (\callback ->
      callAPI' (Left >>> callback) (Right >>> callback) (mkNativeRequest request) *> pure nonCanceler)

mainFlow :: Flow Unit
mainFlow = do
  result <- (runExceptT $ billPayFlow)
  case result of
    Right a -> pure unit
    Left a -> mainFlow

defaultPermissionStatus :: forall e. Array Permission -> Aff (storage :: STORAGE | e) PermissionStatus
defaultPermissionStatus permissions = makeAff (\callback -> (Right >>> callback) PermissionGranted *> pure nonCanceler)

defaultPermissionRequest :: forall e. Array Permission -> Aff (storage :: STORAGE | e) (Array PermissionResponse)
defaultPermissionRequest permissions = makeAff (\callback -> (Right >>> callback) [] *> pure nonCanceler)
