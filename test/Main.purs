module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Presto.Core.Types.App (AppEffects)
import Test.Language.ApiInteractionTest as ApiInteractionTest
import Test.Language.FlowTest as FlowTest
import Test.Language.UIInteractionTest as UIInteractionTest
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run) as T

main :: forall eff. Eff (T.RunnerEffects (AppEffects eff)) Unit
main = T.run [consoleReporter] do
  FlowTest.runTests
  UIInteractionTest.runTests
  ApiInteractionTest.runTests

