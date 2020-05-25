module Presto.Core.Types.Language.APIInteract
  ( apiInteract
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Presto.Core.Types.API (class RestEndpoint, ErrorPayload(..), ErrorResponse, Response(..), Headers, decodeResponse, makeRequest)
import Presto.Core.Types.Language.Interaction (Interaction, request)
import Presto.Core.Utils.Encoding (defaultDecodeJSON)


foreign import _trackException :: String -> String -> String -> String -> String -> String -> Effect Unit

-- Special interact function for API.
apiInteract :: forall a b.
  Encode a => Decode b => RestEndpoint a b
  => a -> Headers -> Interaction (Either ErrorResponse b)
apiInteract a headers = do
  fgnOut <- request (encode (makeRequest a headers))
  pure $ case runExcept (decode fgnOut >>= decodeResponse) of
    -- Try to decode the server's resopnse into the expected type
    Right resp -> do
      Right resp
    Left x -> do
      Left $ case runExcept (decode fgnOut >>= defaultDecodeJSON) of
                       -- See if the server sent an error response, else create our own
                       Right e@(Response _) -> do
                        _ <- getTracker._trackException("api_call")("sdk")("Error")("decode_error")(show e)
                        e
                       Left y -> do
                        _ <- getTracker._trackException("api_call")("sdk")("Error")("decode_error")("Unknown error")
                        Response
                                    { code : 0
                                    , status : ""
                                    , response : ErrorPayload
                                                    { error: true
                                                    , errorMessage: show x <> "\n" <> show y
                                                    , userMessage: "Unknown error"
                                                    }
                                    }
