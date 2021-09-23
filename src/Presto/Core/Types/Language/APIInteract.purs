module Presto.Core.Types.Language.APIInteract
  ( apiInteract
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Debug.Trace (spy)
import Foreign.Class (class Decode, decode, encode)
import Foreign.Generic (decodeJSON)
import Foreign.Object (empty)
import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, ErrorResponse, Headers, Response, makeRequest)
import Presto.Core.Types.Language.Interaction (Interaction, request)

foreign import _trackException :: String -> String -> String -> String -> String -> Unit

-- Special interact function for API.
apiInteract :: forall a b.
  StandardEncode a => Decode b => RestEndpoint a b
  => a -> Headers -> Interaction (Either ErrorResponse (Response b))
apiInteract a headers = do
  -- Using encode Instead of standardEncode, since maybe is already encoded at this stage
  fgnOut <- spy "API RESPONSE" <$> request (encode (makeRequest a headers))
  r <- pure $ do
    (response :: Response String) <- spy "LEVEL 1 PARSE" $ (runExcept $ decode fgnOut)
    (innerResponse :: b) <- runExcept $ decodeJSON response.response
    pure $ response {response = innerResponse}
    -- Try to decode the server's resopnse into the expected type
  pure $ case spy "API RESPONSE PARSE" r of
    Right resp -> Right resp
    Left x -> Left $ 
        case runExcept (decode fgnOut >>= decodeJSON) of
          -- See if the server sent an error response, else create our own
            Right (e :: ErrorResponse) -> do
              let _ = _trackException "api_call" "sdk" "decode_error" "user_errors" $ show e
              e
            Left y -> do
              let _ = _trackException "api_call" "sdk" "decode_error" "user_errors" "Unknown error"
              { code : 0
              , status : ""
              , response : { error: true
                            , errorMessage: show x <> "\n" <> show y
                            , userMessage: "Unknown error"
                            }
              , responseHeaders : empty
              }
