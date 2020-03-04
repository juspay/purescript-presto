module Main where

import Prelude

import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Effect.Aff.AVar (new)
import Effect (Effect)
import Effect.Exception (Error)
import Control.Monad.State.Trans as S
import Data.Either (Either(..))
import Foreign.Class (class Encode, class Decode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Generic.Rep (class Generic)
import Foreign.Object (empty)
import Presto.Core.Flow (class Interact, APIRunner, PermissionRunner(PermissionRunner), Runtime(Runtime), UIRunner, defaultInteract, run, runUI)
import Presto.Core.Types.Language.Flow (Flow)
import Presto.Core.Types.Permission (Permission, PermissionResponse, PermissionStatus(PermissionGranted))

-- FFI to render UI
foreign import showUI' :: forall e. Fn2 (String -> Effect Unit) String (Effect Unit)

-- Presents UI to user, waits to action and recurse.
count :: Int -> Flow Unit
count val = do
  Increment <- runUI $ CounterScreen val
  count $ val + 1

appFlow :: Flow Unit
appFlow = do
  count 0 *> pure unit

launchApp :: Effect Unit
launchApp = do
 let runtime = Runtime uiRunner permissionRunner apiRunner
 let freeFlow = S.evalStateT (run runtime appFlow)
 launchAff_ (new empty >>= freeFlow)

 where
   uiRunner :: UIRunner
   uiRunner a = makeAff (\callback -> runFn2 showUI' (Right >>> callback) a *> pure nonCanceler)

   apiRunner :: APIRunner
   apiRunner request = makeAff (\callback -> (Right >>> callback) "Test" *> pure nonCanceler)

   permissionRunner :: PermissionRunner
   permissionRunner = PermissionRunner defaultPermissionStatus defaultPermissionRequest


main :: Effect Unit
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


defaultPermissionStatus :: forall e. Array Permission -> Aff PermissionStatus
defaultPermissionStatus permissions = makeAff (\callback -> (Right >>> callback) PermissionGranted *> pure nonCanceler)

defaultPermissionRequest :: forall e. Array Permission -> Aff (Array PermissionResponse)
defaultPermissionRequest permissions = makeAff (\callback -> (Right >>> callback) [] *> pure nonCanceler)
