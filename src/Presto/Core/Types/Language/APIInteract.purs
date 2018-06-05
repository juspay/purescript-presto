module Presto.Core.Types.Language.APIInteract
  ( apiInteract
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Bifunctor (bimap)
import Data.Either (Either, either)
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, class Encode, decode, encode)
import Presto.Core.Types.API (class RestEndpoint, ErrorPayload(..), ErrorResponse, Response(..), Headers, decodeResponse, makeRequest)
import Presto.Core.Types.Language.Interaction (Interaction, request)
import Presto.Core.Utils.Encoding (defaultDecodeJSON)

-- Special interact function for API.
apiInteract :: forall a b.
  Encode a => Decode b => RestEndpoint a b
  => a -> Headers -> Interaction (Either ErrorResponse b)
apiInteract a headers = do
  fgnOut <- request (encode (makeRequest a headers))
  pure $ bimap (decodeErrorResponse fgnOut) id (runExcept (decode fgnOut >>= decodeResponse))

-- See if the server sent an error response, else create our own
decodeErrorResponse :: forall a. Show a => Foreign -> a -> Response ErrorPayload
decodeErrorResponse fgnOut x = 
  either (defaultErrorResponse x) id (runExcept (decode fgnOut >>= defaultDecodeJSON))

defaultErrorResponse :: forall a b. Show a => Show b => a -> b -> Response ErrorPayload
defaultErrorResponse x y = Response { code: 0
                      , status: ""
                      , response: ErrorPayload { error: true
                                                , errorMessage: show x <> "\n" <> show y
                                                , userMessage: "Unknown error"
                                                }
                      }