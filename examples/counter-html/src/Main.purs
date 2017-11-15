module Main where

import Prelude

import Control.Monad.Aff (launchAff, makeAff, Aff, Canceler)
import Control.Monad.Aff.AVar (makeVar', AVAR)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (Error, error, EXCEPTION)
import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Free (Free)
import Control.Monad.Loops (allM)
import Control.Monad.State.Trans as S
import Data.Array (zip)
import Data.Either (Either(..))
import Data.Foreign.Class (class Encode, class Decode)
import Data.Foreign.Generic (decodeJSON, defaultOptions, genericDecode, genericEncode)
import Data.Function.Uncurried (runFn2, runFn3, Fn2, Fn3)
import Data.Generic.Rep (class Generic)
import Data.List (fromFoldable)
import Data.StrMap (empty)
import Presto.Core.Flow (PermissionTakeRunner, class Interact, APIRunner, PermissionCheckRunner, PermissionRunner(..), Runtime(..), UIRunner, defaultInteract, run, runUI)
import Presto.Core.Types.API (Header(..), Headers(..), Request(..), URL)
import Presto.Core.Types.App (STORAGE, LOCAL_STORAGE, UI, NETWORK)
import Presto.Core.Types.Language.Flow (FlowWrapper, Flow)
import Presto.Core.Types.Permission (Permission(..), PermissionResponse, PermissionStatus(..), PermissionStatus(..))
import Types (AppEffects, CancelerEffects, EffStorage, AffStorage)

-- FFI to render UI
foreign import showUI' :: forall e. Fn2 (String -> Eff (ui :: UI | e) Unit) String (Eff (ui :: UI | e) Unit)

-- Presents UI to user, waits to action and recurse.
count :: Int -> Flow Unit
count val = do
  Increment <- runUI $ CounterScreen val
  count $ val + 1

appFlow :: Flow Unit
appFlow = do
  count 0 *> pure unit

launchApp :: Eff (AppEffects) (Canceler (CancelerEffects))
launchApp = do
 let runtime = Runtime uiRunner permissionRunner apiRunner
 let freeFlow = S.evalStateT (run runtime appFlow)
 launchAff (makeVar' empty >>= freeFlow)

 where
   uiRunner :: UIRunner
   uiRunner a = makeAff (\err sc -> runFn2 showUI' sc a)

   apiRunner :: APIRunner
   apiRunner request = makeAff (\err sc -> sc "Test")

   permissionRunner :: PermissionRunner
   permissionRunner = PermissionRunner defaultPermissionStatus defaultPermissionRequest


main :: Eff AppEffects (Canceler CancelerEffects)
main = launchApp

-- Creating Screen

data CounterScreen = CounterScreen Int
data CounterScreenAction = Increment

instance counterScreenInteract :: Interact Error CounterScreen CounterScreenAction  where
  interact x = defaultInteract x

derive instance genericCounterScreen :: Generic CounterScreen _
instance encodeCounterScreen :: Encode CounterScreen where
  encode = genericEncode (defaultOptions {unwrapSingleConstructors = false})

derive instance genericCounterScreenAction :: Generic CounterScreenAction _
instance decodeCounterScreenAction :: Decode CounterScreenAction where
  decode = genericDecode (defaultOptions {unwrapSingleConstructors = true})


defaultPermissionStatus :: forall e. Array Permission -> Aff (storage :: STORAGE | e) PermissionStatus
defaultPermissionStatus permissions = makeAff (\err sc -> sc PermissionGranted)

defaultPermissionRequest :: forall e. Array Permission -> Aff (storage :: STORAGE | e) (Array PermissionResponse)
defaultPermissionRequest permissions = makeAff (\err sc -> sc [])
