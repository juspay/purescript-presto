module Presto.Core.Types.Language.APIInteract
  ( apiInteract
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Foreign.Class (class Decode, decode, encode)
import Foreign.Generic (decodeJSON)
import Foreign.Object (empty)
import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, ErrorResponse, Headers, Request(..), Response, makeRequest)
import Presto.Core.Types.Language.Interaction (Interaction, request)

foreign import _trackException :: String -> String -> String -> String -> String -> Unit
foreign import _trackApiCall :: forall a. a -> Unit

-- Special interact function for API.
apiInteract :: forall a b.
  StandardEncode a => Decode b => RestEndpoint a b
  => a -> Headers -> Interaction (Either ErrorResponse (Response b))
apiInteract a headers = do
  let (Request req) = makeRequest a headers
  fgnOut <- request (encode (Request req))
  let _ = if req.logResponse then _trackApiCall fgnOut else unit
  pure $ case runExcept $ decode fgnOut of
    Right (resp :: Response String) -> 
      case runExcept $ decodeJSON resp.response of
        Right (response :: b) -> Right $ { code : resp.code
                                  , responseHeaders : resp.responseHeaders
                                  , response : response
                                  , status : resp.status
                                  }
        Left e -> 
            case runExcept $ decode (encode resp.response) of
              Right (response :: b) -> Right $ { code : resp.code
                                        , responseHeaders : resp.responseHeaders
                                        , response : response
                                        , status : resp.status
                                        }
              Left er -> Left $ { code : resp.code
                        , responseHeaders : resp.responseHeaders
                        , response : { error: true
                                    , errorMessage: resp.response
                                    , userMessage: show er <> "\n" <> resp.response
                                    }
                        , status : resp.status
                        }
    Left x -> Left $ do
        let _ = _trackException "api_call" "sdk" "decode_error" "user_errors" "Unknown error"
        { code : 0
        , status : "FAILURE"
        , response : { error: true
                      , errorMessage: show x
                      , userMessage: "CALL API FAILED" <> show x 
                      }
        , responseHeaders : empty
        }
