module Test.Common where

import Control.Monad.Aff (Aff)
import Prelude (Unit)
import Presto.Core.Types.App (AppEffects)
import Test.Spec (Spec)

type TestCase eff = Aff (AppEffects eff) Unit
type TestFixture eff = Spec (AppEffects eff) Unit
