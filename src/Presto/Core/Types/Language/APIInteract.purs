module Presto.Core.Types.Language.APIInteract
  ( apiInteract
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Foreign.Class (class Decode, class Encode, decode, encode)

import Presto.Core.Types.Language.Interaction (Interaction, request)
import Presto.Core.Types.API (class RestEndpoint, ErrorPayload(..), ErrorResponse, Response(..), Headers, decodeResponse, makeRequest)
import Presto.Core.Utils.Encoding (defaultDecodeJSON)

import Tracker (trackApiCall, trackException) as Tracker
import Tracker.Labels (Label(..)) as Tracker
import Tracker.Types (Level(..), Subcategory(..), Category(API_CALL)) as Tracker

import Presto.Core.Language.Runtime.API(getTime)

-- Special interact function for API.
apiInteract :: forall a b.
  Encode a => Decode b => RestEndpoint a b
  => a -> Headers -> Interaction (Either ErrorResponse b)
apiInteract a headers = do
  startTime <- getTime
  fgnOut <- request (encode (makeRequest a headers))
  pure $ case runExcept (decode fgnOut >>= decodeResponse) of
    -- Try to decode the server's resopnse into the expected type
    Right resp -> do
      _ <- Tracker.trackApiCall Tracker.Sdk Tracker.Info Tracker.DETAILS startTime getTime resp.status (encode resp) r.url r.payload
      Right resp
    Left x -> do
      Left $ case runExcept (decode fgnOut >>= defaultDecodeJSON) of
                       -- See if the server sent an error response, else create our own
                       Right e@(Response _) -> do
                        _ <- Tracker.trackException Tracker.API_CALL Tracker.Sdk Tracker.Error Tracker.DECODE_ERROR (show e)
                        e
                       Left y -> do
                        _ <- Tracker.trackException Tracker.API_CALL Tracker.Sdk Tracker.Error Tracker.DECODE_ERROR ("Unknown error")
                        Response
                                    { code : 0
                                    , status : ""
                                    , response : ErrorPayload
                                                    { error: true
                                                    , errorMessage: show x <> "\n" <> show y
                                                    , userMessage: "Unknown error"
                                                    }
                                    }
