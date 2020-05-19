module Presto.Core.Language.Runtime.API
  ( APIRunner
  , runAPIInteraction
  ) where

import Prelude

import Control.Monad.Except (throwError, runExcept)
import Control.Monad.Free (foldFree)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Exception (error)
import Foreign.Class (encode, decode)

import Presto.Core.Types.API (Request) as API
import Presto.Core.Types.Language.Interaction (InteractionF(..), Interaction, ForeignIn(..), ForeignOut(..))

import Tracker (trackApiCall, trackException) as Tracker
import Tracker.Labels (Label(..)) as Tracker
import Tracker.Types (Level(..), Subcategory(..), Category(API_CALL)) as Tracker

foreign import getTime :: Effect Number

type APIRunner = API.Request -> Aff String

interpretAPI :: APIRunner -> InteractionF ~> Aff
interpretAPI apiRunner r@(Request (ForeignIn fgnIn) nextF) = do
  startTime <- getTime
  case runExcept $ decode fgnIn of
    -- This error should never happen if the `apiInteract` function is made right.
    Left err -> do
     _ <- Tracker.trackException Tracker.API_CALL Tracker.Sdk Tracker.Error Tracker.DECODE_ERROR ("APIInteract is broken" <> show err)
     throwError (error ("apiInteract is broken: " <> show err))
    Right req -> do
    -- trackApiCall :: Subcategory -> Level -> Label -> Int -> Int -> Int -> String -> String -> String -> String -> Effect Unit
    -- sub level label startTime endTime statusC response url payload
      str <- apiRunner req
      _ <- Tracker.trackApiCall Tracker.Sdk Tracker.Info Tracker.DETAILS startTime getTime str.status (encode str) r.url r.payload
      pure $ nextF $ ForeignOut $ encode str


runAPIInteraction :: APIRunner -> Interaction ~> Aff
runAPIInteraction apiRunner = foldFree (interpretAPI apiRunner)
