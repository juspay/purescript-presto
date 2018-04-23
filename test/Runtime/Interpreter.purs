module Test.Runtime.Interpreter where

import Prelude

import Control.Monad.Aff (Aff, delay, forkAff)
import Control.Monad.Aff.AVar (AVAR, AVar, makeEmptyVar, makeVar, putVar, readVar, takeVar)
import Control.Monad.Aff.Console (warn)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throw)
import Control.Monad.Free (foldFree)
import Control.Monad.State.Trans (StateT, get, put, evalStateT, runStateT) as S
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parOneOf)
import Data.Exists (runExists)
import Data.Foreign (Foreign)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (NaturalTransformation)
import Data.Tuple (Tuple(..))
import Presto.Core.Types.App (AppEffects)
import Presto.Core.Types.Language.Flow (ErrorHandler(..), Flow, FlowMethod, FlowWrapper(..), FlowMethodF(..), Control(..))
import Presto.Core.Types.Language.Interaction (ForeignIn(..), ForeignOut(..), Interaction, InteractionF(..))
import Presto.Core.Types.Language.Storage (Key)

type TestStore = Map String String

type St = { maxAcquirePermission :: Int
          , store :: TestStore
          , foreignOutMock :: Maybe Foreign
          }

type InterpreterSt eff a = S.StateT (AVar St) (Aff (AppEffects eff)) a

mkSt :: Int -> TestStore -> St
mkSt a store = {maxAcquirePermission: a, store: store, foreignOutMock: Nothing}

mkStFgn :: Int -> TestStore -> Foreign -> St
mkStFgn a store fgn = {maxAcquirePermission: a, store: store, foreignOutMock: Just fgn}

mkEmptySt :: St
mkEmptySt = mkSt 0 empty

mkStVar :: St -> forall eff. Aff (avar :: AVAR | eff) (AVar St)
mkStVar = makeVar

readSt :: forall eff. InterpreterSt eff St
readSt = S.get >>= (lift <<< readVar)

updateSt :: forall eff. Key -> String -> InterpreterSt eff Unit
updateSt key value = do
  stVar <- S.get
  st <- lift $ takeVar stVar
  let newStore = insert key value st.store
  let st' = mkSt st.maxAcquirePermission newStore
  lift $ putVar st' stVar

runErrorHandler :: forall eff s. ErrorHandler s -> InterpreterSt eff s
runErrorHandler (ThrowError msg) = liftEff $ throw msg
runErrorHandler (ReturnResult res) = pure res

interpretUIInteraction :: forall eff. NaturalTransformation InteractionF (InterpreterSt eff)
interpretUIInteraction (Request (ForeignIn fgnIn) nextF) = do
  st <- readSt
  case st.foreignOutMock of
    Nothing -> liftEff $ throw "Error in UI interaction."
    Just fgnOutMock -> pure $ nextF $ ForeignOut fgnOutMock

runUIInteraction :: forall eff. NaturalTransformation Interaction (InterpreterSt eff)
runUIInteraction = foldFree interpretUIInteraction

interpretAPI :: forall eff. NaturalTransformation InteractionF (InterpreterSt eff)
interpretAPI (Request (ForeignIn fgnIn) nextF) = do
  st <- readSt
  case st.foreignOutMock of
    Nothing -> liftEff $ throw "ForeignOut mock is not set."
    Just fgnOutMock -> pure $ nextF $ ForeignOut fgnOutMock

runAPIInteraction :: forall eff. NaturalTransformation Interaction (InterpreterSt eff)
runAPIInteraction = foldFree interpretAPI

-- TODO: canceller support
forkFlow :: forall a eff. Flow a -> InterpreterSt eff (Control a)
forkFlow flow = do
  stVar <- S.get
  resultVar <- lift makeEmptyVar
  let m = S.evalStateT (run flow) stVar
  _ <- lift $ forkAff $ m >>= flip putVar resultVar
  pure $ Control resultVar


interpret :: forall eff s. NaturalTransformation (FlowMethod s) (InterpreterSt eff)

interpret (RunUI uiInteraction nextF) = do
  runUIInteraction uiInteraction >>= (pure <<< nextF)

interpret (ForkUI uiInteraction next) = do
  void $ runUIInteraction uiInteraction
  pure next

interpret (CallAPI apiInteractionF nextF) =
  runAPIInteraction apiInteractionF >>= (pure <<< nextF)

interpret (Get _ key nextF) = do
  readSt >>= (pure <<< nextF <<< lookup key <<< _.store)

interpret (Set _ key value next) = updateSt key value *> pure next

interpret (Fork flow nextF) = forkFlow flow >>= (pure <<< nextF)

interpret (Await (Control resultVar) nextF) = do
  lift (readVar resultVar) >>= (pure <<< nextF)

interpret (DoAff aff nextF) = lift aff >>= (pure <<< nextF)

interpret (Delay duration next) = lift (delay duration) *> pure next

interpret (OneOf flows nextF) = do
  lift $ warn "oneOf does not work yet"
  st <- S.get
  Tuple a s <- lift $ parOneOf (parFlow st <$> flows)
  S.put s
  pure $ nextF a
  where
    parFlow st flow = S.runStateT (run flow) st

interpret (HandleError flow nextF) =
  run flow >>= runErrorHandler >>= (pure <<< nextF)

interpret _ = liftEff $ throw $ "Interpreter not implemented."

run :: forall eff. NaturalTransformation Flow (InterpreterSt eff)
run = foldFree (\(FlowWrapper x) -> runExists interpret x)
