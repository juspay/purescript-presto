module Presto.Core.Types.Language.APIInteract
  ( apiInteract
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Debug.Trace (spy)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Generic (decodeJSON)
import Foreign.Object (empty)
import Presto.Core.Types.API (class RestEndpoint, ErrorPayload(..), ErrorResponse, Headers, Response(..), decodeResponse, makeRequest)
import Presto.Core.Types.Language.Interaction (Interaction, request)
import Presto.Core.Utils.Encoding (defaultDecodeJSON)

foreign import _trackException :: String -> String -> String -> String -> String -> Unit

-- Special interact function for API.
apiInteract :: forall a b.
  Encode a => Decode b => RestEndpoint a b
  => a -> Headers -> Interaction (Either ErrorResponse b)
apiInteract a headers = do
  fgnOut <- spy "API RESPONSE" <$> request (encode (makeRequest a headers))
  r <- pure $ do
    (Response response :: Response String) <- spy "LEVEL 1 PARSE" $ (runExcept $ decode fgnOut)
    runExcept $ decodeJSON response.response
    -- Try to decode the server's resopnse into the expected type
  pure $ case spy "API RESPONSE PARSE" r of
    Right resp -> Right resp
    Left x -> Left $ 
        case runExcept (decode fgnOut >>= defaultDecodeJSON) of
          -- See if the server sent an error response, else create our own
            Right e@(Response _) -> do
              let _ = _trackException "api_call" "sdk" "decode_error" "user_errors" $ show e
              e
            Left y -> do
              let _ = _trackException "api_call" "sdk" "decode_error" "user_errors" "Unknown error"
              Response { code : 0
                        , status : ""
                        , response : { error: true
                                      , errorMessage: show x <> "\n" <> show y
                                      , userMessage: "Unknown error"
                                      }
                        , responseHeaders : empty
                        }
