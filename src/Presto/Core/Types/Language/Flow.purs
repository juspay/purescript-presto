module Presto.Core.Types.Language.Flow where

import Prelude

import Control.Monad.Free (Free, liftF)
import Data.Either (Either, either)
import Data.Exists (Exists, mkExists)
import Data.Maybe (Maybe)
import Data.Time.Duration (class Duration, Milliseconds, fromDuration)
import Effect.Aff (Aff, makeAff)
import Effect.Aff.AVar as AV
import Effect.Exception (Error)
import Presto.Core.Types.API (class RestEndpoint, class StandardEncode, ErrorResponse, Headers, RegTokens, Response)
import Foreign (Foreign)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (decodeJSON, encodeJSON)
import Foreign.Object as Object
import Presto.Core.Types.Language.APIInteract (apiInteract)
import Presto.Core.Types.Language.Interaction (class Interact, Interaction, interact, interactConv)
import Presto.Core.Types.Language.Storage (Key, class Serializable, serialize, deserialize)
import Presto.Core.Types.Permission (Permission, PermissionStatus, PermissionResponse)

data Authorization = RegistrationTokens RegTokens

type UIResult s = Either Error s
type APIResult s = Either ErrorResponse (Response s)
data Store = LocalStore | InMemoryStore
newtype Control s = Control (AV.AVar s)

type St st = AV.AVar (St' st)

type St' st =
    { state :: st
    , keyMap :: (Object.Object String)
    , logsState :: (Object.Object Foreign)
    }

defaultState :: forall st. st -> St' st
defaultState st
  = { state : st
  , keyMap : Object.empty
  , logsState : Object.empty
  }

data ErrorHandler s
  = ThrowError String
  | ReturnResult s

-- | Algebra of the Flow free language.
data FlowMethodF a st s
  = RunUI (Interaction (UIResult s)) (UIResult s -> a)
  | ForkUI (Interaction (UIResult s)) a
  | CallAPI (Interaction (APIResult s)) (APIResult s -> a)
  | Get Store Key (Maybe String -> a)
  | Set Store Key String a
  | GetForeign (Object.Object Foreign -> a)
  | SetForeign Key Foreign a
  | GetState (st → a)
  | ModifyState (st → st) (st → a)
  | SetState st (Unit → a)
  | Delete Store Key a
  | Fork (Flow st s) (Control s -> a)
  | DoAff (Aff s) (s -> a)
  | Await (Control s) (s -> a)
  | Delay Milliseconds a
  | OneOf (Array (Flow st s)) (s -> a)
  | HandleError (Flow st (ErrorHandler s)) (s -> a)
  | CheckPermissions (Array Permission) (PermissionStatus -> a)
  | TakePermissions (Array Permission) (Array PermissionResponse -> a)

type FlowMethod st s a = FlowMethodF a st s
newtype FlowWrapper st a = FlowWrapper (Exists (FlowMethodF a st))

-- | Free monadic language for making flows.
type Flow st a = Free (FlowWrapper st) a

-- | FlowWrapper for existential type.
wrap :: forall a s st. FlowMethodF a st s -> Flow st a
wrap = liftF <<< FlowWrapper <<< mkExists

-- | Gets some string from state by key
getS :: forall st. Key -> Flow st (Maybe String)
getS key = wrap $ Get InMemoryStore key identity

-- | Puts a string value into state using key.
setS :: forall st.  Key -> String -> Flow st Unit
setS key val = wrap $ Set InMemoryStore key val unit

setLogField :: forall st. Key -> Foreign -> Flow st Unit
setLogField key fgn =
  wrap $ SetForeign key fgn unit

getLogField :: forall st. Key -> Flow st (Maybe Foreign)
getLogField key =
  wrap $ GetForeign (Object.lookup key)

getLogFields :: forall st. Flow st (Object.Object Foreign)
getLogFields =
  wrap $ GetForeign identity

-- | Get the flow state
getState :: forall st. Flow st st
getState = wrap $ GetState identity

-- | Set the flow state
setState :: forall st. st → Flow st Unit
setState st = wrap $ SetState st identity

-- | modify the flow state
modifyState :: forall st. (st → st) → Flow st st
modifyState fn = wrap $ ModifyState fn identity

-- | Deletes a string value from sharedprefs using key.
delete :: forall st. Key -> Flow st Unit
delete key = wrap $ Delete LocalStore key unit

-- | Gets some string from localStorage by key
loadS :: forall st. Key -> Flow st (Maybe String)
loadS key = wrap $ Get LocalStore key identity

-- | Puts a string value into the localStorage using key.
saveS :: forall st. Key -> String -> Flow st Unit
saveS key val = wrap $ Set LocalStore key val unit

-- | Converts error to string and throws at runtime or returns result.
withError :: forall err s st. (err -> String) -> Flow st (Either err s) -> Flow st s
withError toMsg flow = wrap $ HandleError flow' identity
  where
    flow' = flow >>= either (pure <<< ThrowError <<< toMsg) (pure <<< ReturnResult)

-- | Supresses error.
suppress :: forall err s st. Flow st (Either err s) -> Flow st Unit
suppress = void

-- | Throws error.
throwErr :: forall a st. String -> Flow st a
throwErr msg = wrap $ HandleError flow' identity
  where
    flow' = pure $ ThrowError msg

-- | Runs UI and returns result of user's interaction with a screen.
runUI' :: forall a b st. Interact Error a b => a -> Flow st (UIResult b)
runUI' a = wrap $ RunUI (interact a) identity

-- | Runs UI and returns result of user's interaction with a screen.
-- | Handles error in runtime.
runUI :: forall a b st. Interact Error a b => a -> Flow st b
runUI = withError show <<< runUI'

-- | Runs UI async, doesn't return anything useful.
-- | Handles error in runtime.
forkUI :: forall a b st. Interact Error a b => a -> Flow st Unit
forkUI a = wrap $ ForkUI (interact a) unit

-- | Runs UI and doesn't return anything useful (equivalent to `void <<< runUI`).
-- | Handles error in runtime.
showUI :: forall a b st. Interact Error a b => a -> Flow st Unit
showUI = void <<< runUI

-- | Runs UI with a custom converter
-- | Handles error in runtime.
evalUI :: forall a b s st. Interact Error a b => a -> (b -> Either Error s) -> Flow st s
evalUI a from = withError show $ wrap $ RunUI (interactConv a from) identity

-- | Call API being authorized.
callAPI :: forall a b st. StandardEncode a => Decode b => RestEndpoint a b
  => Headers -> a -> Flow st (APIResult b)
callAPI headers a = wrap $ CallAPI (apiInteract a headers) identity

-- | Gets some data from state and deserializes to `s` if possible.
get :: forall s st. Serializable s => Key -> Flow st (Maybe s)
get key = do
  res <- getS key
  pure $ res >>= deserialize

-- | Serializes a value and puts it into the state.
set :: forall s st. Serializable s => Key -> s -> Flow st Unit
set key val = setS key (serialize val)

-- | Forks a flow and returns a control structure for getting results back (like Future).
fork :: forall s st. Flow st s -> Flow st (Control s)
fork flow = wrap $ Fork flow identity

-- | Forks a flow and returns a void control structure.
launch :: forall st. Flow st Unit -> Flow st (Control Unit)
launch flow = wrap $ Fork flow identity

-- | Runs any Aff as part of the flow
doAff :: forall s st. Aff s -> Flow st s
doAff aff = wrap $ DoAff aff identity

-- | Awaits result from a forked flow.
await :: forall s st. Control s -> Flow st s
await control = wrap $ Await control identity

-- | Awaits a forked flow to be completed.
await' :: forall s st. Control s -> Flow st Unit
await' control = void $ wrap $ Await control identity

-- | Delays computation for the given amount of time.
delay :: forall d st. Duration d => d -> Flow st Unit
delay duration = wrap $ Delay (fromDuration duration) unit

-- | Executes a set of actions and returns when the first one is done
oneOf :: forall s st. Array (Flow st s) -> Flow st s
oneOf flows = wrap $ OneOf flows identity

-- | Gets some data from local storage and deserializes to `s` if possible.
load :: forall s st. Serializable s => Key -> Flow st (Maybe s)
load key = do
  res <- loadS key
  pure $ res >>= deserialize

-- | Serializes a value and puts it into the local storage.
save :: forall s st. Serializable s => Key -> s -> Flow st Unit
save key val = saveS key (serialize val)

-- | Checks if permissions granted.
checkPermissions :: forall st. Array Permission -> Flow st PermissionStatus
checkPermissions permissions = wrap $ CheckPermissions permissions identity

-- | Tries to aquire permissions.
takePermissions :: forall st. Array Permission -> Flow st (Array PermissionResponse)
takePermissions permissions = wrap $ TakePermissions permissions identity
