module Core where

import Prelude

import Control.Monad.Aff (Aff, Canceler, launchAff, makeAff)
import Control.Monad.Aff.AVar (makeVar')
import Control.Monad.Eff (Eff)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.State.Trans as S
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn2)
import Data.StrMap (empty)
import Engineering.Helpers.Commons (callAPI', mkNativeRequest, showUI')
import Engineering.Types.App (AppEffects, CancelerEffects)
import Presto.Core.Flow (APIRunner, Flow, PermissionRunner(..), Runtime(..), UIRunner, run)
import Presto.Core.Types.App (STORAGE)
import Presto.Core.Types.Permission (Permission, PermissionResponse, PermissionStatus(..))
import Product.BillPay (billPayFlow)

launchApp :: Eff (AppEffects) (Canceler (CancelerEffects))
launchApp = do
  let runtime = Runtime uiRunner permissionRunner apiRunner
  let freeFlow = S.evalStateT (run runtime mainFlow)
  launchAff (makeVar' empty >>= freeFlow)
  where
    uiRunner :: UIRunner
    uiRunner a = makeAff (\err sc -> runFn2 showUI' sc a)

    permissionRunner :: PermissionRunner
    permissionRunner = PermissionRunner defaultPermissionStatus defaultPermissionRequest

    apiRunner :: APIRunner
    apiRunner request = makeAff (\err sc -> callAPI' err sc (mkNativeRequest request))

mainFlow :: Flow Unit
mainFlow = do
  result <- (runExceptT $ billPayFlow)
  case result of
    Right a -> pure unit
    Left a -> mainFlow

defaultPermissionStatus :: forall e. Array Permission -> Aff (storage :: STORAGE | e) PermissionStatus
defaultPermissionStatus permissions = makeAff (\err sc -> sc PermissionGranted)

defaultPermissionRequest :: forall e. Array Permission -> Aff (storage :: STORAGE | e) (Array PermissionResponse)
defaultPermissionRequest permissions = makeAff (\err sc -> sc [])
