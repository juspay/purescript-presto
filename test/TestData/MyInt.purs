module Test.TestData.MyInt where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Int (fromString)
import Presto.Core.Types.Language.Storage (class Serializable)

newtype MyInt = MyInt Int

instance myIntSerializable :: Serializable MyInt where
  serialize (MyInt i) = show i
  deserialize i = fromString i >>= Just <<< MyInt

instance myIntEq :: Eq MyInt where
  eq (MyInt a) (MyInt b) = a == b

instance myIntShow :: Show MyInt where
  show (MyInt a) = "MyInt " <> show a
