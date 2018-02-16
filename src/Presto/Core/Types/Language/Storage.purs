module Presto.Core.Types.Language.Storage
  ( class Serializable
  , Key
  , deserialize
  , serialize
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (decodeJSON, encodeJSON)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

type Key = String

class Serializable a where
  serialize :: a -> String
  deserialize :: String -> Maybe a

instance stringSerializable :: Serializable String where
  serialize = id
  deserialize = Just

primDeserialize :: forall a. Decode a => String -> Maybe a
primDeserialize = decodeJSON >>> runExcept >>> either (const Nothing) Just

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
  serialize = map serialize >>> encodeJSON
  deserialize a = primDeserialize a >>= traverse deserialize
