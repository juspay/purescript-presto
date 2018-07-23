module Test.Language.ApiInteractionTest where

import Prelude

import Control.Monad.State.Trans as S
import Effect.Aff (Aff)
import Foreign.Class (class Decode, class Encode, encode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq as GEq
import Data.Generic.Rep.Show as GShow
import Data.Map (empty)
import Global.Unsafe (unsafeStringify)
import Presto.Core.Types.API (Request(..)) as API
import Presto.Core.Types.API (class RestEndpoint, ErrorResponse, Headers(..), Method(..), defaultDecodeResponse)
import Presto.Core.Types.Language.Flow (Flow, callAPI, suppress, withError)
import Presto.Core.Utils.Encoding (defaultEncodeJSON, defaultEncode, defaultDecode)
import Test.Runtime.Interpreter (run, mkStFgn, mkStVar)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

-- Sample
newtype SendOtpReq = SendOtpReq { mobileNumber :: String }
newtype SendOtpResp = SendOtpResp { code :: Int }

derive instance genericSendOtpReq :: Generic SendOtpReq _
instance decodeSendOtpReq :: Decode SendOtpReq where decode = defaultDecode
instance encodeSendOtpReq :: Encode SendOtpReq where encode = defaultEncode
instance showSendOtpReq :: Show SendOtpReq where
  show = GShow.genericShow

derive instance genericSendOtpResp :: Generic SendOtpResp _
instance decodeSendOtpResp :: Decode SendOtpResp where decode = defaultDecode
instance encodeSendOtpResp :: Encode SendOtpResp where encode = defaultEncode
instance showSendOtpResp :: Show SendOtpResp where
  show = GShow.genericShow
instance eqSendOtpResp :: Eq SendOtpResp where
  eq = GEq.genericEq

instance restEndpointSendOtpReq :: RestEndpoint SendOtpReq SendOtpResp where
  makeRequest reqBody headers =
    API.Request
        { method: POST
        , url: "/api/v1/sendOtp"
        , headers: headers
        , payload: defaultEncodeJSON reqBody
        }
  decodeResponse body = defaultDecodeResponse body

sendOtpWithSuppressFlow :: Flow Unit
sendOtpWithSuppressFlow = suppress $ callAPI (Headers []) (SendOtpReq { mobileNumber: "+5 555 55 55"})

errToStr :: ErrorResponse -> String
errToStr resp = show resp

sendOtpWithErrorFlow :: Flow SendOtpResp
sendOtpWithErrorFlow =
  withError errToStr $ callAPI (Headers []) (SendOtpReq { mobileNumber: "+5 555 55 55"})

apiInteractionWithErrorTest :: Aff Unit
apiInteractionWithErrorTest = do
  stVar <- mkStVar $ mkStFgn 0 empty (encode $ unsafeStringify (SendOtpResp {code: 1}))
  x <- S.evalStateT (run sendOtpWithErrorFlow) stVar
  x `shouldEqual` (SendOtpResp {code: 1})

apiInteractionSuppressErrorTest :: Aff Unit
apiInteractionSuppressErrorTest = do
  stVar <- mkStVar $ mkStFgn 0 empty (encode $ unsafeStringify (SendOtpResp {code: 1}))
  S.evalStateT (run sendOtpWithSuppressFlow) stVar

runTests :: Spec Unit
runTests = do
  describe "API Interaction test" do
    it "API Interaction with error test" apiInteractionWithErrorTest
    it "API Interaction with suppressing of error test" apiInteractionSuppressErrorTest
