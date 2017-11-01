module Presto.Core.Language.Runtime.API
  ( APIRunner
  , runAPIInteraction
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError, runExcept)
import Control.Monad.Free (foldFree)
import Data.Either (Either(..))
import Data.Foreign.Class (encode, decode)
import Data.NaturalTransformation (NaturalTransformation)
import Presto.Core.Types.API (Request) as API
import Presto.Core.Types.Language.Interaction (InteractionF(..), Interaction, ForeignIn(..), ForeignOut(..))

type APIRunner = forall e. API.Request -> Aff e String

interpretAPI :: forall eff. APIRunner -> NaturalTransformation InteractionF (Aff eff)
interpretAPI apiRunner (Request (ForeignIn fgnIn) nextF) = do
  case runExcept $ decode fgnIn of
    -- This error should never happen if the `apiInteract` function is made right.
    Left err -> throwError (error ("apiInteract is broken: " <> show err))
    Right req -> do
      str <- apiRunner req
      pure $ nextF $ ForeignOut $ encode str


runAPIInteraction :: forall eff. APIRunner -> NaturalTransformation Interaction (Aff eff)
runAPIInteraction apiRunner = foldFree (interpretAPI apiRunner)
