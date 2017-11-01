module Presto.Core.Types.Language.Flow where

import Prelude

import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Free (Free, liftF)
import Data.Either (Either, either)
import Data.Exists (Exists, mkExists)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds)
import Presto.Core.Types.API (class RestEndpoint, ErrorResponse, Headers, RegTokens)
import Presto.Core.Types.App (AppFlow)
import Presto.Core.Types.Language.APIInteract (apiInteract)
import Presto.Core.Types.Language.Interaction (class Interact, Interaction, interact, interactConv)
import Presto.Core.Types.Language.Storage (Key, class Serializable, serialize, deserialize)
import Presto.Core.Types.Permission (Permission, PermissionStatus, PermissionResponse)

data Authorization = RegistrationTokens RegTokens

type UIResult s = Either Error s
type APIResult s = Either ErrorResponse s
data Store = LocalStore | InMemoryStore
newtype Control s = Control (AVar s)

data ErrorHandler s
  = ThrowError String
  | ReturnResult s

-- | Algebra of the Flow free language.
data FlowMethodF a s
  = RunUI (Interaction (UIResult s)) (UIResult s -> a)
  | ForkUI (Interaction (UIResult s)) a
  | CallAPI (Interaction (APIResult s)) (APIResult s -> a)
  | Get Store Key (Maybe String -> a)
  | Set Store Key String a
  | Fork (Flow s) (Control s -> a)
  | DoAff (forall eff. AppFlow eff s) (s -> a)
  | Await (Control s) (s -> a)
  | Delay Milliseconds a
  | OneOf (Array (Flow s)) (s -> a)
  | HandleError (Flow (ErrorHandler s)) (s -> a)
  | CheckPermissions (Array Permission) (PermissionStatus -> a)
  | TakePermissions (Array Permission) (Array PermissionResponse -> a)

type FlowMethod s a = FlowMethodF a s
newtype FlowWrapper a = FlowWrapper (Exists (FlowMethodF a))

-- | Free monadic language for making flows.
type Flow a = Free FlowWrapper a

-- | FlowWrapper for existential type.
wrap :: forall a s. FlowMethodF a s -> Flow a
wrap = liftF <<< FlowWrapper <<< mkExists

-- | Gets some string from state by key
getS :: Key -> Flow (Maybe String)
getS key = wrap $ Get InMemoryStore key id

-- | Puts a string value into state using key.
setS :: Key -> String -> Flow Unit
setS key val = wrap $ Set InMemoryStore key val unit

-- | Gets some string from localStorage by key
loadS :: Key -> Flow (Maybe String)
loadS key = wrap $ Get LocalStore key id

-- | Puts a string value into the localStorage using key.
saveS :: Key -> String -> Flow Unit
saveS key val = wrap $ Set LocalStore key val unit

-- | Converts error to string and throws at runtime or returns result.
withError :: forall err s. (err -> String) -> Flow (Either err s) -> Flow s
withError toMsg flow = wrap $ HandleError flow' id
  where
    flow' = flow >>= either (pure <<< ThrowError <<< toMsg) (pure <<< ReturnResult)

-- | Supresses error.
suppress :: forall err s. Flow (Either err s) -> Flow Unit
suppress = void

-- | Throws error.
throwErr :: forall a. String -> Flow a
throwErr msg = wrap $ HandleError flow' id
  where
    flow' = pure $ ThrowError msg

-- | Runs UI and returns result of user's interaction with a screen.
runUI' :: forall a b. Interact Error a b => a -> Flow (UIResult b)
runUI' a = wrap $ RunUI (interact a) id

-- | Runs UI and returns result of user's interaction with a screen.
-- | Handles error in runtime.
runUI :: forall a b. Interact Error a b => a -> Flow b
runUI = withError show <<< runUI'

-- | Runs UI async, doesn't return anything useful.
-- | Handles error in runtime.
forkUI :: forall a b. Interact Error a b => a -> Flow Unit
forkUI a = wrap $ ForkUI (interact a) unit

-- | Runs UI and doesn't return anything useful (equivalent to `void <<< runUI`).
-- | Handles error in runtime.
showUI :: forall a b. Interact Error a b => a -> Flow Unit
showUI = void <<< runUI

-- | Runs UI with a custom converter
-- | Handles error in runtime.
evalUI :: forall a b s. Interact Error a b => a -> (b -> Either Error s) -> Flow s
evalUI a from = withError show $ wrap $ RunUI (interactConv a from) id

-- | Call API being authorized.
callAPI :: forall a b. Encode a => Decode b => RestEndpoint a b
  => Headers -> a -> Flow (APIResult b)
callAPI headers a = wrap $ CallAPI (apiInteract a headers) id

-- | Gets some data from state and deserializes to `s` if possible.
get :: forall s. Serializable s => Key -> Flow (Maybe s)
get key = do
  res <- getS key
  pure $ res >>= deserialize

-- | Serializes a value and puts it into the state.
set :: forall s. Serializable s => Key -> s -> Flow Unit
set key val = setS key (serialize val)

-- | Forks a flow and returns a control structure for getting results back (like Future).
fork :: forall s. Flow s -> Flow (Control s)
fork flow = wrap $ Fork flow id

-- | Forks a flow and returns a void control structure.
launch :: Flow Unit -> Flow (Control Unit)
launch flow = wrap $ Fork flow id

-- | Runs any Aff as part of the flow
doAff :: forall s. (forall eff. AppFlow eff s) -> Flow s
doAff aff = wrap $ DoAff aff id

-- | Awaits result from a forked flow.
await :: forall s. Control s -> Flow s
await control = wrap $ Await control id

-- | Awaits a forked flow to be completed.
await' :: forall s. Control s -> Flow Unit
await' control = void $ wrap $ Await control id

-- | Delays computation for a given number of milliseconds.
delay :: Milliseconds -> Flow Unit
delay duration = wrap $ Delay duration unit

-- | Executes a set of actions and returns when the first one is done
oneOf :: forall s. Array (Flow s) -> Flow s
oneOf flows = wrap $ OneOf flows id

-- | Gets some data from local storage and deserializes to `s` if possible.
load :: forall s. Serializable s => Key -> Flow (Maybe s)
load key = do
  res <- loadS key
  pure $ res >>= deserialize

-- | Serializes a value and puts it into the local storage.
save :: forall s. Serializable s => Key -> s -> Flow Unit
save key val = saveS key (serialize val)

-- | Checks if permissions granted.
checkPermissions :: Array Permission -> Flow PermissionStatus
checkPermissions permissions = wrap $ CheckPermissions permissions id

-- | Tries to aquire permissions.
takePermissions :: Array Permission -> Flow (Array PermissionResponse)
takePermissions permissions = wrap $ TakePermissions permissions id
