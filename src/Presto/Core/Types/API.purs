module Presto.Core.Types.API
  ( class RestEndpoint
  , class EncodeRecord
  , class StandardEncode
  , class EncodeWithOptions
  , Method(..)
  , Header(..)
  , HeaderField
  , HeaderValue
  , Headers(..)
  , RegTokens(..)
  , Request(..)
  , Response(..)
  , ErrorResponse
  , ErrorPayload
  , URL
  , RestAPIOptions(..)
  , defaultMakeRequest
  , defaultMakeRequestWithoutLogs
  , defaultMakeRequestString
  , defaultMakeRequest_
  , defaultMakeRequestWithFallback
  , defaultDecodeResponse
  , makeRequest
  , decodeResponse
  , encodeRecordWithOptions
  , encodeWithOptions
  , responsePayload
  , encodeRequest
  , standardEncode
  , standardEncodeJSON
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Foreign (F, Foreign, unsafeToForeign)
import Foreign.Class (class Decode, class Encode, encode)
import Data.Identity (Identity)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype,unwrap)
import Foreign.Generic.Class (class GenericDecode, defaultOptions, Options)
import Prim.Row (class Cons)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.RowList (class RowToList, Nil, Cons)
import Foreign.NullOrUndefined (undefined)
import Foreign.Object (Object)
import Foreign.Object as Object
import Presto.Core.Utils.Encoding (defaultDecode, defaultEncode, defaultDecodeJSON, unsafeStringify)
import Unsafe.Coerce (unsafeCoerce)
import Record as Record
import Type.Proxy (Proxy(..))

class RestEndpoint a b | a -> b where
  makeRequest :: a -> Headers -> Request
  decodeResponse :: String -> F b
  encodeRequest :: a -> Foreign

standardEncodeJSON :: forall a. StandardEncode a => a -> String
standardEncodeJSON = unsafeStringify <<< standardEncode

defaultMakeRequest :: forall a x. RestEndpoint a x => Method -> URL -> Headers -> a -> Maybe RestAPIOptions -> Request
defaultMakeRequest method url headers req restAPIOptions =
  Request { method:  method
          , url: url
          , headers: headers
          , payload: unsafeStringify $ encodeRequest req
          , logResponse: true
          , options: restAPIOptions
          , fallbackUrls : []
          }

defaultMakeRequestWithFallback :: forall req resp. RestEndpoint req resp => Method -> Domain -> Headers -> req -> Maybe RestAPIOptions -> Array Domain -> (Domain -> URL) -> Request
defaultMakeRequestWithFallback method baseUrl headers req restAPIOptions fallbackBaseURL mkURL =
    Request { method
            , url : mkURL baseUrl
            , headers
            , payload: unsafeStringify $ encodeRequest req
            , logResponse: true
            , options: restAPIOptions
            , fallbackUrls : mkURL <$> fallbackBaseURL
            }


defaultMakeRequestWithoutLogs :: forall a x. RestEndpoint a x => Method -> URL -> Headers -> a -> Maybe RestAPIOptions -> Request
defaultMakeRequestWithoutLogs method url headers req restAPIOptions =
  Request { method:  method
          , url: url
          , headers: headers
          , payload: unsafeStringify $ encodeRequest req
          , logResponse: false
          , options: restAPIOptions
          , fallbackUrls : []
          }

defaultMakeRequestString :: Method -> String -> Headers -> String -> Maybe RestAPIOptions -> Request
defaultMakeRequestString method url headers req restAPIOptions =
  Request { method:  method
          , url: url
          , headers: headers
          , payload: req
          , logResponse: true
          , options: restAPIOptions
          , fallbackUrls : []
          }

defaultMakeRequest_ :: Method -> URL -> Headers -> Maybe RestAPIOptions ->Request
defaultMakeRequest_ method url headers restAPIOptions = Request { method:  method
                                                 , url: url
                                                 , headers: headers
                                                 , payload: ""
                                                 , logResponse: true
                                                 , options: restAPIOptions
                                                 , fallbackUrls : []
                                                 }

defaultDecodeResponse :: forall a x. Generic a x => GenericDecode x
                      => String -> F a
defaultDecodeResponse = defaultDecodeJSON

type RegTokens =
  { regToken :: String
  , tokenId :: String
  , loginToken :: String
  }

type HeaderField = String
type HeaderValue = String
data Header = Header HeaderField HeaderValue
newtype Headers = Headers (Array Header)

type URL = String
type Domain = String -- just for naming clarity

data Method = POST | GET | PUT | DELETE | HEAD
data GetReqBody = GetReqBody

newtype Request = Request
  { method :: Method
  , url :: URL
  , payload :: String
  , headers :: Headers
  , logResponse :: Boolean
  , options :: Maybe RestAPIOptions
  , fallbackUrls :: Array String
  }

derive instance newtypeRequest :: Newtype Request _
newtype RestAPIOptions = RestAPIOptions
  { connectionTimeout :: Maybe Int
  , readTimeout :: Maybe Int
  }

type ErrorResponse = Response ErrorPayload

type ErrorPayload = { error :: Boolean
                    , errorMessage :: String
                    , userMessage :: String
                    }

type Response a =
  { code :: Int
  , status :: String
  , response :: a
  , responseHeaders :: Object (Array String)
  }

responsePayload :: forall a. Response a -> a
responsePayload r = r.response

derive instance genericMethod :: Generic Method _
instance encodeMethod :: Encode Method where
  encode = defaultEncode
instance decodeMethod :: Decode Method where
  decode = defaultDecode
instance showMethod :: Show Method where
  show POST = "POST"
  show GET = "GET"
  show PUT = "PUT"
  show DELETE = "DELETE"
  show HEAD = "HEAD"

derive instance genericHeader :: Generic Header _
instance encodeHeaderG :: Encode Header where
  encode = defaultEncode
instance decodeHeaderG :: Decode Header where
  decode = defaultDecode

derive instance genericHeaders :: Generic Headers _
instance encodeHeadersG :: Encode Headers where
  encode = defaultEncode
instance decodeHeadersG :: Decode Headers where
  decode = defaultDecode

derive instance genericRequest :: Generic Request _
instance encodeRequestG :: Encode Request where
  encode = defaultEncode
instance decodeRequestG :: Decode Request where
  decode = defaultDecode

derive instance genericRestAPIOptions :: Generic RestAPIOptions _
instance encodeRestAPIOptions :: Encode RestAPIOptions where
  encode = defaultEncode
instance decodeRestAPIOptions :: Decode RestAPIOptions where
  decode = defaultDecode

derive instance genericGetReqBody :: Generic GetReqBody _
instance decodeGetReqBody :: Decode GetReqBody where decode = defaultDecode
instance encodeGetReqBody :: Encode GetReqBody where encode = defaultEncode


-- | The `StandardEncode` class is a proxy to `Encode`
-- | of the form `a -> Foreign` using `generics-rep` deriving.
-- |
-- | Requirement to proxy is to enable Maybe to encode into undefined, 
-- | instead of null
class StandardEncode a where
  standardEncode :: a -> Foreign

instance voidstandardEncode :: StandardEncode Void where
  standardEncode = encode

instance unitstandardEncode :: StandardEncode Unit where
  standardEncode = encode

instance foreignstandardEncode :: StandardEncode Foreign where
  standardEncode = encode

instance stringstandardEncode :: StandardEncode String where
  standardEncode = encode

instance charstandardEncode :: StandardEncode Char where
  standardEncode = encode

instance booleanstandardEncode :: StandardEncode Boolean where
  standardEncode = encode

instance numberstandardEncode :: StandardEncode Number where
  standardEncode = encode

instance intstandardEncode :: StandardEncode Int where
  standardEncode = encode

instance identitystandardEncode :: StandardEncode a => StandardEncode (Identity a) where
  standardEncode = standardEncode <<< unwrap

instance arraystandardEncode :: StandardEncode a => StandardEncode (Array a) where
  standardEncode = unsafeToForeign <<< map standardEncode

instance maybestandardEncode :: StandardEncode a => StandardEncode (Maybe a) where
  standardEncode = maybe undefined standardEncode

instance objectstandardEncode :: StandardEncode v => StandardEncode (Object v) where
  standardEncode = unsafeToForeign <<< Object.mapWithKey (\_ -> standardEncode)

instance recordstandardEncode :: (RowToList r rl, EncodeRecord r rl) => StandardEncode (Record r) where
  standardEncode = encodeWithOptions defaultOptions

class EncodeWithOptions a where
  encodeWithOptions :: Options -> a -> Foreign

instance encodeWithOptionsRecord :: (RowToList r rl, EncodeRecord r rl) => EncodeWithOptions (Record r) where
  encodeWithOptions opts = unsafeToForeign <<< encodeRecordWithOptions (Proxy :: Proxy rl) opts

else instance encodeWithOptionsOther :: StandardEncode a => EncodeWithOptions a where
  encodeWithOptions _ = standardEncode

class EncodeRecord :: forall k. Row Type -> k -> Constraint
class EncodeRecord r rl | rl -> r where
  encodeRecordWithOptions :: Proxy rl -> Options -> Record r -> Object Foreign

instance encodeRecordNil :: EncodeRecord () Nil where
  encodeRecordWithOptions _ _ _ = Object.empty

instance encodeRecordCons
    :: ( Cons l a r_ r
       , EncodeRecord r_ rl_
       , IsSymbol l
       , EncodeWithOptions a
       )
    => EncodeRecord r (Cons l a rl_)
  where
    encodeRecordWithOptions _ opts rec =
      let obj = encodeRecordWithOptions (Proxy :: Proxy rl_) opts (unsafeCoerce rec)
          l = reflectSymbol (Proxy :: Proxy l)
       in Object.insert (opts.fieldTransform l) (encodeWithOptions opts (Record.get (Proxy :: Proxy l) rec)) obj