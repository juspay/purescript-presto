module Test.Language.UIInteractionTest where

import Prelude
import Control.Monad.State.Trans as S
import Data.Map (empty)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic as GShow
import Data.Generic.Rep.Eq as GEq
import Effect.Aff (Aff)
import Effect.Exception (error, Error)
import Foreign.Class (class Encode, class Decode, encode)
import Foreign.Generic (defaultOptions, genericEncode)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Presto.Core.Utils.Encoding (defaultDecode)
import Presto.Core.Types.Language.Interaction (class Interact, defaultInteract)
import Presto.Core.Types.Language.Flow (Flow, evalUI, runUI)
import Test.Runtime.Interpreter (run, mkStFgn, mkStVar)

data StaticQRScreen = StaticQRScreen String
data StaticQRScreenAction = StaticQRScreenAbort | StaticQRScreenAction String

instance interactionStaticQrScreen :: Interact Error StaticQRScreen StaticQRScreenAction where
  interact a = defaultInteract a
instance showStaticQRScreenAction :: Show StaticQRScreenAction where
  show = GShow.genericShow
instance eqStaticQRScreenAction :: Eq StaticQRScreenAction where
  eq = GEq.genericEq

derive instance genericStaticQRScreen :: Generic StaticQRScreen _
instance encodeStaticQRScreen :: Encode StaticQRScreen where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = false })

derive instance genericStaticQRScreenAction :: Generic StaticQRScreenAction _
instance decodeStaticQRScreenAction :: Decode StaticQRScreenAction where
  decode = defaultDecode
instance encodeStaticQRScreenAction :: Encode StaticQRScreenAction where
  encode = genericEncode (defaultOptions { unwrapSingleConstructors = false })

qrFlow :: Flow StaticQRScreenAction
qrFlow = runUI $ StaticQRScreen "ABC"

qrConvertFlow :: Flow String
qrConvertFlow = evalUI (StaticQRScreen "ABC") from
  where
    from (StaticQRScreenAction x) = Right x
    from StaticQRScreenAbort = Left $ error "Invalid Action"

uiInteractionTest :: Aff Unit
uiInteractionTest = do
  stVar <- mkStVar $ mkStFgn 0 empty (encode (StaticQRScreenAction "ABC"))
  x <- S.evalStateT (run qrFlow) stVar
  x `shouldEqual` (StaticQRScreenAction "ABC")

uiInteractionWithConvertTest :: Aff Unit
uiInteractionWithConvertTest = do
  stVar <- mkStVar $ mkStFgn 0 empty (encode (StaticQRScreenAction "ABC"))
  x <- S.evalStateT (run qrConvertFlow) stVar
  x `shouldEqual` "ABC"

runTests :: Spec Unit
runTests = do
  describe "UI Interaction test" do
    it "UI Interaction test" uiInteractionTest
    it "UI Interaction with convert test" uiInteractionWithConvertTest
