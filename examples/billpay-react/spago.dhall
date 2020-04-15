{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "presto-app"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
  , "effect"
  , "folds"
  , "foreign-generic"
  , "free"
  , "newtype"
  , "prelude"
  , "presto"
  , "psci-support"
  , "transformers"
  ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
