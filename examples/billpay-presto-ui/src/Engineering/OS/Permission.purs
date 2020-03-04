module Engineering.OS.Permission where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Data.Array (zip, foldl)
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (Error, error)
import Foreign.Generic (decodeJSON)
import Presto.Core.Types.Language.Flow (Flow, checkPermissions, takePermissions)
import Presto.Core.Types.Permission (Permission(..), PermissionResponse, PermissionStatus(..))

foreign import getPermissionStatus' :: Fn3 (Error -> EffStorage Unit) (String -> EffStorage Unit) String (EffStorage Unit)
foreign import requestPermission' :: Fn3 (Error -> EffStorage Unit) (String -> EffStorage Unit) String (EffStorage Unit)

type EffStorage = Effect
type AffStorage = Aff

toAndroidPermission :: Permission -> String
toAndroidPermission PermissionSendSms = "android.permission.READ_SMS"
toAndroidPermission PermissionReadPhoneState = "android.permission.READ_PHONE_STATE"
toAndroidPermission PermissionWriteStorage = "android.permission.WRITE_EXTERNAL_STORAGE"
toAndroidPermission PermissionReadStorage = "android.permission.READ_EXTERNAL_STORAGE"
toAndroidPermission PermissionCamera = "android.permission.CAMERA"
toAndroidPermission PermissionLocation = "android.permission.LOCATION"
toAndroidPermission PermissionCoarseLocation = "android.permission.ACCESS_COARSE_LOCATION"
toAndroidPermission PermissionContacts = "android.permission.CONTACTS"

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

getPermissionStatus :: Permission -> AffStorage Boolean
getPermissionStatus permission = do
  permissionStr <- makeAff (\callback -> do
    runFn3 getPermissionStatus' (Left >>> callback) (Right >>> callback) (toAndroidPermission permission)
    pure nonCanceler)
  case (runExcept (decodeJSON permissionStr)) of
    Right x -> pure x
    Left err -> throwError (error (show err))

checkIfPermissionsGranted :: Array Permission -> AffStorage PermissionStatus
checkIfPermissionsGranted permissions = do
  pStatus <- sequence $ getPermissionStatus <$> permissions
  let res = foldl (\acc x -> if acc == false then false else x) true pStatus
  pure $ if res
    then PermissionGranted
    else PermissionDeclined

requestPermissions :: Array Permission -> AffStorage (Array PermissionResponse)
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
