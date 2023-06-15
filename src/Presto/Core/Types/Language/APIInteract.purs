module Presto.Core.Types.Language.APIInteract
  ( apiInteract
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (length, (!!))
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.Newtype (modify)
import Foreign.Class (class Decode, decode, encode)
import Foreign.Generic (decodeJSON)
import Foreign.Object (empty)
import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, ErrorResponse, Headers, Request(..), Response, URL, makeRequest)
import Presto.Core.Types.Language.Interaction (Interaction, request)

foreign import _trackException :: String -> String -> String -> String -> String -> Unit
foreign import _trackApiCall :: forall a. a -> Unit

-- Special interact function for API.
apiInteract :: forall a b.
  StandardEncode a => Decode b => RestEndpoint a b
  => a -> Headers -> Int -> Interaction (Either ErrorResponse (Response b))
apiInteract a headers retryCount = do
  let getUrl req = fromMaybe req.url $ ([req.url] <> req.fallbackUrls) !! retryCount

  let (Request req) = modify (\ req -> req {url = getUrl req}) $ makeRequest a headers

  fgnOut <- request (encode (Request req))
  let _ = if req.logResponse then _trackApiCall fgnOut else unit
  case runExcept $ decode fgnOut of
    Right (resp :: Response String) -> 
      case runExcept $ decodeJSON resp.response of
        Right (response :: b) -> pure $ Right { code : resp.code
                                  , responseHeaders : resp.responseHeaders
                                  , response : response
                                  , status : resp.status
                                  }
        Left _ -> 
            case runExcept $ decode (encode resp.response) of
              Right (response :: b) -> pure $ Right { code : resp.code
                                        , responseHeaders : resp.responseHeaders
                                        , response : response
                                        , status : resp.status
                                        }
              Left er -> do 
                let errorResp = { code : resp.code
                                , responseHeaders : resp.responseHeaders
                                , response : { error: true
                                            , errorMessage: resp.response
                                            , userMessage: show er <> "\n" <> resp.response
                                            }
                                , status : resp.status
                                }
                retryWithFallbackUrl req.fallbackUrls errorResp
    Left x -> do 
        let _ = _trackException "api_call" "sdk" "decode_error" "user_errors" "Unknown error"
            errorResp = { code : 0
                       , status : "FAILURE"
                       , response : { error: true
                                     , errorMessage: show x
                                     , userMessage: "CALL API FAILED" <> show x 
                                     }
                       , responseHeaders : empty
                       }
        retryWithFallbackUrl req.fallbackUrls errorResp
                              

    where
       retryWithFallbackUrl :: Array URL -> ErrorResponse -> Interaction (Either ErrorResponse (Response b))
       retryWithFallbackUrl fallbackUrls currErrorResponse
            | retryCount >= length fallbackUrls = pure $ Left currErrorResponse
            | otherwise = apiInteract a headers (retryCount + 1)
             