module Presto.Core.Language.Runtime.Interpreter
  ( Runtime(..)
  , UIRunner
  , PermissionCheckRunner
  , PermissionTakeRunner
  , PermissionRunner(..)
  , run
  ) where

import Prelude

import Control.Monad.Except (throwError, runExcept)
import Control.Monad.Free (foldFree)
import Control.Monad.State.Trans as S
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parOneOf)
import Data.Either (Either(..))
import Data.Exists (runExists)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, killFiber)
import Effect.Aff.AVar as AV
import Effect.Exception (Error, error)
import Foreign.JSON (parseJSON)
import Foreign.Object as Object
import Presto.Core.Utils.Encoding (unsafeStringify)
import Presto.Core.Language.Runtime.API (APIRunner, runAPIInteraction)
import Presto.Core.LocalStorage (deleteValueFromLocalStore, getValueFromLocalStore, setValueToLocalStore)
import Presto.Core.Types.Language.Flow (ErrorHandler(..), Flow, FlowMethod, FlowMethodF(..), FlowWrapper(..), Store(..), Control(..), St)
import Presto.Core.Types.Language.Interaction (InteractionF(..), Interaction, ForeignOut(..))
import Presto.Core.Types.Language.Storage (Key)
import Presto.Core.Types.Permission (Permission, PermissionResponse, PermissionStatus)

type AffError = (Error -> Effect Unit)
type AffSuccess s = (s -> Effect Unit)

type InterpreterSt st a = S.StateT (St st) Aff a

type UIRunner = String -> Aff String

type PermissionCheckRunner = Array Permission -> Aff PermissionStatus
type PermissionTakeRunner = Array Permission -> Aff (Array PermissionResponse)
data PermissionRunner = PermissionRunner PermissionCheckRunner PermissionTakeRunner

data Runtime = Runtime UIRunner PermissionRunner APIRunner


readState :: forall a. InterpreterSt a a
readState = S.get >>= (lift <<< map (_.state) <<< AV.read)

readMap :: forall a. InterpreterSt a (Object.Object String)
readMap = S.get >>= (lift <<< map (_.keyMap) <<< AV.read)

updateState :: forall a. a -> InterpreterSt a Unit
updateState value = do
  stVar <- S.get
  state <- lift $ AV.take stVar
  lift $ AV.put (state {state =  value}) stVar

updateMap :: forall a. Key -> String -> InterpreterSt a Unit
updateMap key value = do
  stVar <- S.get
  state <- lift $ AV.take stVar
  let st' = Object.insert key value state.keyMap
  lift $ AV.put (state {keyMap =  st'}) stVar

interpretUI :: UIRunner -> InteractionF ~> Aff
interpretUI uiRunner (Request fgnIn nextF) = do
  json <- uiRunner $ unsafeStringify fgnIn
  case (runExcept (parseJSON json)) of
    Right fgnOut -> pure $ nextF $ ForeignOut fgnOut
    Left err -> throwError $ error $ show err

runUIInteraction :: UIRunner -> Interaction ~> Aff
runUIInteraction uiRunner = foldFree (interpretUI uiRunner)

forkFlow :: forall a e. Runtime -> Flow e a -> InterpreterSt e (Control a)
forkFlow rt flow = do
  st <- S.get
  resultVar <- lift AV.empty
  let m = S.evalStateT (run rt flow) st
  fiber <- lift $ forkAff $ m >>= (\a -> AV.put a resultVar *> pure a)
  pure $ Control fiber resultVar

runErrorHandler :: forall st s. ErrorHandler s -> InterpreterSt st s
runErrorHandler (ThrowError msg) = throwError $ error msg
runErrorHandler (ReturnResult res) = pure res

interpret :: forall s e. Runtime -> FlowMethod e s ~> InterpreterSt e
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
  readMap >>= (Object.lookup key >>> next >>> pure)

interpret _ (GetState next) = do
  readState <#> next

interpret _ (Set LocalStore key value next) = do
  lift $ setValueToLocalStore key value
  pure next

interpret _ (SetState value next) = do
  updateState value *> pure (next unit)
  
interpret _ (Set InMemoryStore key value next) = do
  updateMap key value *> pure next

interpret _ (GetForeign next) =
  S.get >>= (lift <<< map (next <<< (_.logsState)) <<< AV.read)

interpret _ (SetForeign key fValue next) = do
  stVar <- S.get
  state <- lift $ AV.take stVar
  let stf' = Object.insert key fValue state.logsState
  lift $ AV.put (state {logsState = stf'}) stVar
  pure next

interpret _ (Delete LocalStore key next) = do
  lift $ deleteValueFromLocalStore key
  pure next

interpret _ (Delete InMemoryStore key next) = do
  _ <- Object.delete key <$> readMap
  pure next

interpret _ (ModifyState fn next) = do
  stVar <- S.get
  state <- lift $ AV.take stVar
  let st' = fn state.state
  lift $ AV.put (state {state = st'}) stVar
  pure $ next st'

interpret r (Fork flow nextF) = forkFlow r flow >>= (pure <<< nextF)

interpret _ (DoAff aff nextF) = lift aff >>= (pure <<< nextF)

interpret _ (Await (Control _ resultVar) nextF) = do
  lift (AV.read resultVar) >>= (pure <<< nextF)

interpret _ (Kill (Control fiber _) next) = do
  lift (killFiber (error "Received termination") fiber)
  pure (next unit)

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

run :: forall st. Runtime -> Flow st ~> InterpreterSt st
run runtime = foldFree (\(FlowWrapper x) -> runExists (interpret runtime) x)
