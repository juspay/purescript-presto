module Test.Main where

import Prelude

import Effect (Effect)
import Test.Language.ApiInteractionTest as ApiInteractionTest
import Test.Language.FlowTest as FlowTest
import Test.Language.UIInteractionTest as UIInteractionTest
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run) as T

main :: Effect Unit
main = T.run [consoleReporter] do
  FlowTest.runTests
  UIInteractionTest.runTests
  ApiInteractionTest.runTests

