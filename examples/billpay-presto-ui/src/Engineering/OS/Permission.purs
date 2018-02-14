module Engineering.OS.Permission where

import Prelude

import Control.Monad.Aff (Aff, makeAff, nonCanceler)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Loops (allM)
import Data.Array (zip)
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Foreign.Generic (decodeJSON)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.List (fromFoldable)
import Data.Tuple (Tuple(..))
import Presto.Core.Types.App (STORAGE)
import Presto.Core.Types.Language.Flow (Flow, checkPermissions, takePermissions)
import Presto.Core.Types.Permission (Permission(..), PermissionResponse, PermissionStatus(..))

foreign import getPermissionStatus' :: forall e. Fn3 (Error -> EffStorage e Unit) (String -> EffStorage e Unit) String (EffStorage e Unit)
foreign import requestPermission' :: forall e. Fn3 (Error -> EffStorage e Unit) (String -> EffStorage e Unit) String (EffStorage e Unit)

type EffStorage e = Eff (storage :: STORAGE | e)
type AffStorage e = Aff (storage :: STORAGE | e)

toAndroidPermission :: Permission -> String
toAndroidPermission PermissionSendSms = "android.permission.READ_SMS"
toAndroidPermission PermissionReadPhoneState = "android.permission.READ_PHONE_STATE"
toAndroidPermission PermissionWriteStorage = "android.permission.WRITE_EXTERNAL_STORAGE"
toAndroidPermission PermissionReadStorage = "android.permission.READ_EXTERNAL_STORAGE"

allPermissionGranted :: Array PermissionResponse -> Boolean
allPermissionGranted = all (\(Tuple _ status) -> status == PermissionGranted)

getStoragePermission :: Flow Boolean
getStoragePermission =
  ifM (storageGranted) (pure true) (askForStorage)
  where
    storageGranted :: Flow Boolean
    storageGranted = do
    	 status <- checkPermissions [PermissionWriteStorage]
    	 case status of
    	 	PermissionGranted -> pure true
    		_ -> pure false
    askForStorage :: Flow Boolean
    askForStorage = pure <<< allPermissionGranted =<< takePermissions [PermissionWriteStorage]

storagePermissionGranted :: Flow Boolean
storagePermissionGranted = do
	 status <- checkPermissions [PermissionWriteStorage]
	 case status of
	 	PermissionGranted -> pure true
		_ -> pure false

getPermissionStatus :: forall e. Permission -> AffStorage e Boolean
getPermissionStatus permission = do
  permissionStr <- makeAff (\callback -> do
    runFn3 getPermissionStatus' (Left >>> callback) (Right >>> callback) (toAndroidPermission permission)
    pure nonCanceler)
  case (runExcept (decodeJSON permissionStr)) of
    Right x -> pure x
    Left err -> throwError (error (show err))

checkIfPermissionsGranted :: forall e. Array Permission -> AffStorage e PermissionStatus
checkIfPermissionsGranted permissions = do
  check <- allM getPermissionStatus $ fromFoldable permissions
  pure $ if check
    then PermissionGranted
    else PermissionDeclined

requestPermissions :: forall e. Array Permission -> AffStorage e (Array PermissionResponse)
requestPermissions permissions = do
  response <- makeAff (\callback -> do
    runFn3 requestPermission' (Left >>> callback) (Right >>> callback) $ show jPermission
    pure nonCanceler)
  case runExcept $ decodeJSON response of
    Right (statuses :: Array Boolean) -> pure $ zip permissions (map toResponse statuses)
    Left err -> throwError (error (show err))
  where
    toResponse wasGranted = if wasGranted then PermissionGranted else PermissionDeclined
    jPermission = map toAndroidPermission permissions
