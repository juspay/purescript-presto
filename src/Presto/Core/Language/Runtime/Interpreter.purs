module Presto.Core.Language.Runtime.Interpreter
  ( Runtime(..)
  , UIRunner
  , PermissionCheckRunner
  , PermissionTakeRunner
  , PermissionRunner(..)
  , run
  ) where

import Prelude

import Control.Monad.Aff (Aff, forkAff, delay)
import Control.Monad.Aff.AVar (AVar, makeEmptyVar, takeVar, putVar, readVar)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Except (throwError, runExcept)
import Control.Monad.Free (foldFree)
import Control.Monad.State.Trans as S
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parOneOf)
import Data.Either (Either(..))
import Data.Exists (runExists)
import Data.Foreign.JSON (parseJSON)
import Data.NaturalTransformation (NaturalTransformation)
import Data.StrMap (StrMap, insert, lookup)
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeStringify)
import Presto.Core.Language.Runtime.API (APIRunner, runAPIInteraction)
import Presto.Core.LocalStorage (getValueFromLocalStore, setValueToLocalStore)
import Presto.Core.Types.App (AppFlow, STORAGE, UI)
import Presto.Core.Types.Language.Flow (ErrorHandler(..), Flow, FlowMethod, FlowMethodF(..), FlowWrapper(..), Store(..), Control(..))
import Presto.Core.Types.Language.Interaction (InteractionF(..), Interaction, ForeignOut(..))
import Presto.Core.Types.Language.Storage (Key)
import Presto.Core.Types.Permission (Permission, PermissionResponse, PermissionStatus)

type AffError e = (Error -> Eff e Unit)
type AffSuccess s e = (s -> Eff e Unit)

type St = AVar (StrMap String)
type InterpreterSt eff a = S.StateT St (AppFlow eff) a

type UIRunner = forall e. String -> Aff (ui :: UI | e) String

type PermissionCheckRunner = forall e. Array Permission -> Aff (storage :: STORAGE | e) PermissionStatus
type PermissionTakeRunner = forall e. Array Permission -> Aff (storage :: STORAGE | e) (Array PermissionResponse)
data PermissionRunner = PermissionRunner PermissionCheckRunner PermissionTakeRunner

data Runtime = Runtime UIRunner PermissionRunner APIRunner

-- FIXME: can the effects on the interepreter of each type be more fine-grained?

readState :: forall eff. InterpreterSt eff (StrMap String)
readState = S.get >>= (lift <<< readVar)

updateState :: forall eff. Key -> String -> InterpreterSt eff Unit
updateState key value = do
  stVar <- S.get
  st <- lift $ takeVar stVar
  let st' = insert key value st
  lift $ putVar st' stVar

interpretUI :: forall eff. UIRunner -> NaturalTransformation InteractionF (AppFlow eff)
interpretUI uiRunner (Request fgnIn nextF) = do
  json <- uiRunner $ unsafeStringify fgnIn
  case (runExcept (parseJSON json)) of
    Right fgnOut -> pure $ nextF $ ForeignOut fgnOut
    Left err -> throwError $ error $ show err

runUIInteraction :: forall eff. UIRunner -> NaturalTransformation Interaction (AppFlow eff)
runUIInteraction uiRunner = foldFree (interpretUI uiRunner)

-- TODO: canceller support
forkFlow :: forall eff a. Runtime -> Flow a -> InterpreterSt eff (Control a)
forkFlow rt flow = do
  st <- S.get
  resultVar <- lift makeEmptyVar
  let m = S.evalStateT (run rt flow) st
  _ <- lift $ forkAff $ m >>= flip putVar resultVar
  pure $ Control resultVar

runErrorHandler :: forall eff s. ErrorHandler s -> InterpreterSt eff s
runErrorHandler (ThrowError msg) = throwError $ error msg
runErrorHandler (ReturnResult res) = pure res

interpret :: forall eff s. Runtime -> NaturalTransformation (FlowMethod s) (InterpreterSt eff)
interpret (Runtime _ _ apiRunner) (CallAPI apiInteractionF nextF) = do
  lift $ runAPIInteraction apiRunner apiInteractionF
    >>= (pure <<< nextF)

interpret (Runtime uiRunner _ _) (RunUI uiInteraction nextF) = do
  lift $ runUIInteraction uiRunner uiInteraction
    >>= (pure <<< nextF)

interpret (Runtime uiRunner _ _) (ForkUI uiInteraction next) = do
  void $ lift $ forkAff $ runUIInteraction uiRunner uiInteraction
  pure next

interpret _ (Get LocalStore key next) = lift $ getValueFromLocalStore key >>= (pure <<< next)

interpret _ (Get InMemoryStore key next) = do
  readState >>= (lookup key >>> next >>> pure)

interpret _ (Set LocalStore key value next) = do
  lift $ setValueToLocalStore key value
  pure next

interpret _ (Set InMemoryStore key value next) = do
  updateState key value *> pure next

interpret r (Fork flow nextF) = forkFlow r flow >>= (pure <<< nextF)

interpret _ (DoAff aff nextF) = lift aff >>= (pure <<< nextF)

interpret _ (InitUIWithScreen uiFlow nextF) = lift uiFlow >>= (pure <<< nextF)

interpret _ (InitUI uiFlow nextF) = lift uiFlow >>= (pure <<< nextF)

interpret _ (RunScreen uiFlow nextF) = lift uiFlow >>= (pure <<< nextF)

interpret _ (ForkScreen uiFlow nextF) = do
  void $ lift $ forkAff uiFlow
  pure nextF

interpret _ (Await (Control resultVar) nextF) = do
  lift (readVar resultVar) >>= (pure <<< nextF)

interpret _ (Delay duration next) = lift (delay duration) *> pure next

interpret rt (OneOf flows nextF) = do
  st <- S.get
  Tuple a s <- lift $ parOneOf (parFlow st <$> flows)
  S.put s
  pure $ nextF a
  where
    parFlow st flow = S.runStateT (run rt flow) st

interpret rt (HandleError flow nextF) =
  run rt flow >>= runErrorHandler >>= (pure <<< nextF)

interpret (Runtime _ (PermissionRunner check _) _) (CheckPermissions permissions nextF) = do
  lift $ check permissions >>= (pure <<< nextF)

interpret (Runtime _ (PermissionRunner _ take) _) (TakePermissions permissions nextF) = do
  lift $ take permissions >>= (pure <<< nextF)

run :: forall eff. Runtime -> NaturalTransformation Flow (InterpreterSt eff)
run runtime = foldFree (\(FlowWrapper x) -> runExists (interpret runtime) x)
