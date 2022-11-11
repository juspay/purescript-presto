module Presto.Core.Types.Permission
  ( Permission(..)
  , PermissionResponse
  , PermissionStatus(..)
  ) where

import Prelude

import Data.Tuple (Tuple)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)

data PermissionStatus = PermissionGranted
                      | PermissionDeclined
                      | PermissionDeclinedForever

derive instance eqPermissionStatus  :: Eq PermissionStatus

data Permission = PermissionReadPhoneState
                | PermissionSendSms
                | PermissionReadStorage
                | PermissionWriteStorage
                | PermissionCamera
                | PermissionLocation
                | PermissionCoarseLocation
                | PermissionContacts

type PermissionResponse = Tuple Permission PermissionStatus

derive instance genericPermission  :: Generic Permission _
instance encodePermission :: Encode Permission where
  encode = genericEncode defaultOptions
instance decodePermission :: Decode Permission where
  decode = genericDecode defaultOptions
instance showPermissionInstance :: Show Permission where
  show = genericShow
