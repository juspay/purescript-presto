module Presto.Core.Types.Language.Storage
  ( class Serializable
  , Key
  , deserialize
  , serialize
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (decodeJSON, encodeJSON)
import Data.Maybe (Maybe(..))
import Data.Traversable (intercalate, traverse)
import Global.Unsafe (unsafeStringify)

type Key = String

class Serializable a where
  serialize :: a -> String
  deserialize :: String -> Maybe a

instance stringSerializable :: Serializable String where
  serialize = id
  deserialize = Just

primDeserialize :: forall a. Decode a => String -> Maybe a
primDeserialize = decodeJSON >>> runExcept >>> hush

instance booleanSerializable :: Serializable Boolean where
  serialize = encodeJSON
  deserialize = primDeserialize

instance intSerializable :: Serializable Int where
  serialize = encodeJSON
  deserialize = primDeserialize

instance numberSerializable :: Serializable Number where
  serialize = encodeJSON
  deserialize = primDeserialize

instance charSerializable :: Serializable Char where
  serialize = encodeJSON
  deserialize = primDeserialize

instance arraySerializable :: Serializable a => Serializable (Array a) where
  serialize a = "[" <> (intercalate "," (serialize <$> a)) <> "]"
  deserialize a = (toStringArray <$> toForeignArray a) >>= deserializeArray
    where
      toForeignArray :: String -> Maybe (Array Foreign)
      toForeignArray = decodeJSON >>> runExcept >>> hush

      toStringArray :: Array Foreign -> Array String
      toStringArray = map unsafeStringify

      deserializeArray :: Array String -> Maybe (Array a)
      deserializeArray = traverse deserialize
