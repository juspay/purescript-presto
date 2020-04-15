module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Language.ApiInteractionTest as ApiInteractionTest
import Test.Language.FlowTest as FlowTest
import Test.Language.UIInteractionTest as UIInteractionTest
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec) as T

main :: Effect Unit
main = launchAff_ $ T.runSpec [consoleReporter] do
  FlowTest.runTests
  UIInteractionTest.runTests
  ApiInteractionTest.runTests
