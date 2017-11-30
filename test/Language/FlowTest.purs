module Test.Language.FlowTest where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff as Aff
import Control.Monad.Aff.AVar (readVar)
import Control.Monad.State.Trans (runStateT, evalStateT)
import Data.Map (member, empty, insert, singleton)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Presto.Core.Flow (class Serializable, Flow, get, set, load, save, fork, await, await', doAff, delay, oneOf)
import Presto.Core.Operators (onFirstRun, inParallel)
import Presto.Core.Types.Language.Flow (Store(..))
import Test.Common (TestCase, TestFixture)
import Test.Runtime.Interpreter (mkEmptySt, mkSt, mkStVar, run)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual)
import Test.TestData.MyInt (MyInt(..))
import Test.TestData.Some (Some(..))

data MobileNumber = MobileNumber String

mobileNumberKey :: String
mobileNumberKey = "MobileNumber"

instance mobileNumberSerializable :: Serializable MobileNumber where
  serialize (MobileNumber i) = i
  deserialize i = Just $ MobileNumber i
instance mobileNumberShow :: Show MobileNumber where
  show (MobileNumber n) = n
instance mobileNumberEq :: Eq MobileNumber where
  eq (MobileNumber n1) (MobileNumber n2) = n1 == n2

getSomeFlow :: Store -> Flow (Maybe Some)
getSomeFlow LocalStore = load "some"
getSomeFlow InMemoryStore = get "some"

getMobilePhoneFlow :: Store -> Flow (Maybe MobileNumber)
getMobilePhoneFlow LocalStore = load mobileNumberKey
getMobilePhoneFlow InMemoryStore = get mobileNumberKey

getSomeAndMobilePhoneFlow :: Store -> Flow (Tuple (Maybe Some) (Maybe MobileNumber))
getSomeAndMobilePhoneFlow store = Tuple <$> getSomeFlow store <*> getMobilePhoneFlow store

getTest :: forall eff. Store -> TestCase eff
getTest store = do
  let store1 = insert "some" "abc" empty
  let store2 = insert mobileNumberKey "+5 555 555 555" store1
  let s = mkSt 0 store2
  stVar1 <- mkStVar s
  stVar2 <- mkStVar s
  stVar3 <- mkStVar s

  mbSome1 <- evalStateT (run $ getSomeFlow store) stVar1
  mbSome1 `shouldEqual` (Just $ Some {someField: "abc"})

  mbMobileNumber1 <- evalStateT (run $ getMobilePhoneFlow store) stVar2
  mbMobileNumber1 `shouldEqual` (Just $ MobileNumber "+5 555 555 555")

  (Tuple mbSome2 mbMobileNumber2) <- evalStateT (run $ getSomeAndMobilePhoneFlow store) stVar3
  mbSome2 `shouldEqual` (Just $ Some {someField: "abc"})
  mbMobileNumber2 `shouldEqual` (Just $ MobileNumber "+5 555 555 555")

setGet :: Store -> Some -> Flow (Maybe Some)
setGet LocalStore some = do
  save "some2" some
  _ :: Maybe Some <- load ""
  load "some2"

setGet InMemoryStore some = do
  set "some2" some
  _ :: Maybe Some <- get ""
  get "some2"

setGetTest :: forall eff. Store -> TestCase eff
setGetTest store = do
  stVar <- mkStVar $ mkSt 0 empty
  let m = setGet store (Some {someField: "this"})
  mbSomeResult <- evalStateT (run m) stVar
  mbSomeResult `shouldEqual` (Just $ Some {someField: "this"})

onFirstRunFlow1 :: Flow MyInt
onFirstRunFlow1 = onFirstRun "42" $ pure $ MyInt 42

onFirstRunFlow2 :: Flow MyInt
onFirstRunFlow2 = onFirstRun "42" $ pure $ MyInt 444444

onFirstRunFlowTest :: forall eff. TestCase eff
onFirstRunFlowTest = do
  stVar <- mkStVar mkEmptySt
  Tuple r1 st1 <- runStateT (run onFirstRunFlow1) stVar
  store1 <- readVar st1 >>= (pure <<< _.store)
  let expected1 = "42" `member` store1
  expected1 `shouldEqual` true
  r1 `shouldEqual` (MyInt 42)
  Tuple r2 st2 <- runStateT (run onFirstRunFlow2) st1
  r2 `shouldEqual` (MyInt 42)

childFlow :: String -> Flow String
childFlow msg = delay (Milliseconds 500.0) *> pure msg

parentFlow :: Flow String
parentFlow = do
  child1 <- fork (childFlow "ABCD")
  child2 <- fork (childFlow "EFGH")
  (<>) <$> await child1 <*> await child2

forkAwaitTest :: forall eff. TestCase eff
forkAwaitTest = do
  stVar <- mkStVar mkEmptySt
  Tuple r _ <- runStateT (run parentFlow) stVar
  r `shouldEqual` "ABCDEFGH"

doAffTest :: forall eff. TestCase eff
doAffTest = do
  stVar <- mkStVar mkEmptySt
  r <- evalStateT (run affFlow) stVar
  r `shouldEqual` true
  where
    affFlow = doAff do
       Aff.delay (Milliseconds 100.0)
       pure true

-- | Awaiting twice should return the same result.

parentFlow2 :: Flow String
parentFlow2 = do
  child1 <- fork (childFlow "ABCD")
  _ <- fork (childFlow "EFGH")
  (<>) <$> await child1 <*> await child1

awaitTwiceTest :: forall eff. TestCase eff
awaitTwiceTest = do
  stVar <- mkStVar mkEmptySt
  Tuple r _ <- runStateT (run parentFlow2) stVar
  r `shouldEqual` "ABCDABCD"

updateFlow :: Int -> String -> Flow Unit
updateFlow 0 _ = pure unit
updateFlow n k = do
  mbStored <- load k
  case mbStored of
    Nothing -> save k k
    Just stored -> save k (stored <> k)
  updateFlow (n - 1) k

concurrentUpdateFlow :: Flow (Tuple (Maybe String) (Maybe String))
concurrentUpdateFlow = do
  inParallel [updateFlow 5 "A", updateFlow 10 "B"] >>= (traverse_ await')
  Tuple <$> load "A" <*> load "B"

storeConcurrencyTest :: forall eff. TestCase eff
storeConcurrencyTest = do
  stVar <- mkStVar mkEmptySt
  Tuple (Tuple mbA mbB) _ <- runStateT (run concurrentUpdateFlow) stVar
  mbA `shouldEqual` (Just "AAAAA")
  mbB `shouldEqual` (Just "BBBBBBBBBB")

oneOfTest :: forall eff. TestCase eff
oneOfTest = do
  stVar <- mkStVar $ mkSt 0 $ singleton "initialState" "xxx"
  Tuple (Tuple mbA mbB) stVar' <- runStateT (run oneOfUpdateFlow) stVar
  -- At least one should be resolved to our satisfaction
  [Just "AAAAA", Just "BBBBBBBBBB"] `shouldContain` (mbA <|> mbB)
  -- And the state should not be lost
  stNew <- readVar stVar'
  stNew.store `shouldContain` "xxx"
  where
    oneOfUpdateFlow = do
      oneOf [updateFlow 5 "A", updateFlow 10 "B"]
      Tuple <$> load "A" <*> load "B"

runTests :: forall eff. TestFixture eff
runTests = do
  describe "Flow language test" do
    it "OnFirstRun test" onFirstRunFlowTest
    it "Get LocalStore test" $ getTest LocalStore
    it "Set/Get LocalStore test" $ setGetTest LocalStore
    it "Get InMemoryStore test " $ getTest InMemoryStore
    it "Set/Get InMemoryStore test" $ setGetTest InMemoryStore
    it "Fork/Await test" forkAwaitTest
    it "Run Aff test" doAffTest
    it "Await twice test" awaitTwiceTest
    it "Store concurrency test" storeConcurrencyTest
    it "OneOf concurrency test" oneOfTest
