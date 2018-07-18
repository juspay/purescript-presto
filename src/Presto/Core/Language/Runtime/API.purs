module Presto.Core.Language.Runtime.API
  ( APIRunner
  , runAPIInteraction
  ) where

import Prelude

import Effect.Aff (Aff)
import Effect.Exception (error)
import Control.Monad.Except (throwError, runExcept)
import Control.Monad.Free (foldFree)
import Data.Either (Either(..))
import Data.NaturalTransformation (NaturalTransformation)
import Foreign.Class (encode, decode)
import Presto.Core.Types.API (Request) as API
import Presto.Core.Types.Language.Interaction (InteractionF(..), Interaction, ForeignIn(..), ForeignOut(..))

type APIRunner = API.Request -> Aff String

interpretAPI :: APIRunner -> NaturalTransformation InteractionF Aff
interpretAPI apiRunner (Request (ForeignIn fgnIn) nextF) = do
  case runExcept $ decode fgnIn of
    -- This error should never happen if the `apiInteract` function is made right.
    Left err -> throwError (error ("apiInteract is broken: " <> show err))
    Right req -> do
      str <- apiRunner req
      pure $ nextF $ ForeignOut $ encode str


runAPIInteraction :: APIRunner -> NaturalTransformation Interaction Aff
runAPIInteraction apiRunner = foldFree (interpretAPI apiRunner)
