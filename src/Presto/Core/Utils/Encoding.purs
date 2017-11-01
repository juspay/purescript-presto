module Presto.Core.Utils.Encoding
  ( defaultDecode
  , defaultEncode
  , defaultEnumDecode
  , defaultEnumEncode
  , defaultDecodeJSON
  , defaultEncodeJSON
  ) where

import Prelude

import Data.Foreign (Foreign, F)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericDecodeJSON, genericEncode, genericEncodeJSON)
import Data.Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import Data.Foreign.Generic.EnumEncoding (class GenericDecodeEnum, class GenericEncodeEnum, genericDecodeEnum, genericEncodeEnum)
import Data.Foreign.Generic.Types (Options)
import Data.Generic.Rep (class Generic)

options :: Options
options = defaultOptions { unwrapSingleConstructors = true }

defaultDecode :: forall a b. Generic a b => GenericDecode b => Foreign -> F a
defaultDecode x = genericDecode options x

defaultEncode ::  forall a b. Generic a b => GenericEncode b => a -> Foreign
defaultEncode x = genericEncode options x

defaultEnumDecode :: forall a b. Generic a b => GenericDecodeEnum b => Foreign -> F a
defaultEnumDecode x = genericDecodeEnum { constructorTagTransform: id } x

defaultEnumEncode ::  forall a b. Generic a b => GenericEncodeEnum b => a -> Foreign
defaultEnumEncode x = genericEncodeEnum { constructorTagTransform: id } x

defaultDecodeJSON :: forall a b. Generic a b => GenericDecode b => String -> F a
defaultDecodeJSON x = genericDecodeJSON options x

defaultEncodeJSON ::  forall a b. Generic a b => GenericEncode b => a -> String
defaultEncodeJSON x = genericEncodeJSON options x
