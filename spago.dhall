{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "presto"
, dependencies =
  [ "aff"
  , "avar"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "exceptions"
  , "exists"
  , "foreign-generic"
  , "foreign-object"
  , "free"
  , "generics-rep"
  , "prelude"
  , "psci-support"
  , "record"
  , "spec"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
