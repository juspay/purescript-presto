module Test.TestData.Some where

import Prelude
import Control.Monad.Eff.Exception (Error)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Generic.Rep (class Generic) as G
import Data.Maybe (Maybe(..))

import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode)
import Presto.Core.Types.Language.Storage (class Serializable)
import Presto.Core.Types.Language.Interaction (class Interact, defaultInteract)

newtype Some = Some { someField :: String }

data SomeScreen = SomeScreen
data SomeScreenAction = ReturnSome Some

derive instance genericSome  :: G.Generic Some _
derive instance genericSomeScreen  :: G.Generic SomeScreen _
derive instance genericSomeScreenAction  :: G.Generic SomeScreenAction _
instance encodeSomeScreen :: Encode SomeScreen where
  encode = defaultEncode

instance decodeSome :: Decode Some where
  decode = defaultDecode
instance decodeSomeScreenAction :: Decode SomeScreenAction where
  decode = defaultDecode

instance someScreen :: Interact Error SomeScreen SomeScreenAction where
  interact x = defaultInteract x

instance showSome :: Show Some where
  show (Some {someField}) = "Some " <> someField

instance eqSome :: Eq Some where
  eq (Some a1) (Some a2) = a1.someField == a2.someField

instance serializableSome :: Serializable Some where
  serialize (Some s) = s.someField
  deserialize s = Just $ Some {someField: s}
