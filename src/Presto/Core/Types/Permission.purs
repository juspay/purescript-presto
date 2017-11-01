module Presto.Core.Types.Permission
  ( Permission(..)
  , PermissionResponse
  , PermissionStatus(..)
  ) where

import Prelude
import Data.Tuple (Tuple)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)

data PermissionStatus = PermissionGranted
                      | PermissionDeclined

derive instance eqPermissionStatus  :: Eq PermissionStatus

data Permission = PermissionReadPhoneState
                | PermissionSendSms
                | PermissionReadStorage
                | PermissionWriteStorage

type PermissionResponse = Tuple Permission PermissionStatus

derive instance genericPermission  :: Generic Permission _
instance encodePermission :: Encode Permission where
  encode = genericEncode defaultOptions
instance decodePermission :: Decode Permission where
  decode = genericDecode defaultOptions
instance showPermissionInstance :: Show Permission where
  show = showPermission

showPermission :: Permission -> String
showPermission PermissionReadPhoneState = "PermissionReadPhoneState"
showPermission PermissionSendSms = "PermissionSendSms"
showPermission PermissionReadStorage = "PermissionReadStorage"
showPermission PermissionWriteStorage = "PermissionWriteStorage"
