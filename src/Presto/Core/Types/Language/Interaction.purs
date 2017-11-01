module Presto.Core.Types.Language.Interaction
  ( class Interact,
    interact,
    Interaction,
    request,
    interactConv,
    defaultInteract,
    InteractionF(..),
    ForeignIn(..),
    ForeignOut(..)
  ) where

import Prelude
import Control.Monad.Free (Free, liftF)
import Control.Monad.Except (runExcept)
import Control.Monad.Eff.Exception (error, Error)
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode, class Encode, encode, decode)
import Data.Either (Either(..))

newtype ForeignIn = ForeignIn Foreign
newtype ForeignOut = ForeignOut Foreign

data InteractionF next
  = Request ForeignIn (ForeignOut -> next)

-- public interface to Interaction:

-- | Free monadic Interaction type. Denotes a request-response interaction.
type Interaction s = Free InteractionF s

-- | Represents a relation between types `a` (input type) and `b` (output type).
class (Encode a, Decode b) <= Interact err a b | a -> b, b -> a where
  interact :: a -> Interaction (Either err b)

request :: Foreign -> Interaction Foreign
request fgnIn = do
  ForeignOut fgnOut <- liftF $ Request (ForeignIn fgnIn) id
  pure fgnOut

interactConv :: forall a b s err. Interact err a b =>
  a -> (b -> Either err s) -> Interaction (Either err s)
interactConv a from = do
  eitherB <- interact a
  pure $ eitherB >>= from

defaultInteract :: forall a b. Encode a => Decode b
                => a -> Interaction (Either Error b)
defaultInteract a = do
  fgnOut <- request $ encode a
  case (runExcept (decode fgnOut)) of
    Right y -> pure $ Right y
    Left x -> pure $ Left $ error $ show x
