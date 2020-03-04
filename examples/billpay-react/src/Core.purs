module Core where

import Prelude

import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Effect.Aff.AVar (new)
import Effect (Effect)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.State.Trans as S
import Data.Either (Either(..))
import Data.Function.Uncurried (runFn2)
import Foreign.Object (empty)
import Engineering.Helpers.Commons (callAPI', mkNativeRequest, showUI')
import Presto.Core.Flow (APIRunner, Flow, PermissionRunner(..), Runtime(..), UIRunner, run)
import Presto.Core.Types.Permission (Permission, PermissionResponse, PermissionStatus(..))
import Product.BillPay (billPayFlow)

launchApp :: Effect Unit
launchApp = do
  let runtime = Runtime uiRunner permissionRunner apiRunner
  let freeFlow = S.evalStateT (run runtime mainFlow)
  launchAff_ (new empty >>= freeFlow)
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

defaultPermissionStatus :: Array Permission -> Aff PermissionStatus
defaultPermissionStatus permissions = makeAff (\callback -> (Right >>> callback) PermissionGranted *> pure nonCanceler)

defaultPermissionRequest :: Array Permission -> Aff (Array PermissionResponse)
defaultPermissionRequest permissions = makeAff (\callback -> (Right >>> callback) [] *> pure nonCanceler)
