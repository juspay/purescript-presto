let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.3/packages.dhall
        sha256:ffc496e19c93f211b990f52e63e8c16f31273d4369dbae37c7cf6ea852d4442f

in  upstream
  with foreign-generic =
    { dependencies =
      [ "arrays"
      , "assert"
      , "bifunctors"
      , "console"
      , "control"
      , "effect"
      , "either"
      , "exceptions"
      , "foldable-traversable"
      , "foreign"
      , "foreign-object"
      , "identity"
      , "lists"
      , "maybe"
      , "newtype"
      , "partial"
      , "prelude"
      , "record"
      , "strings"
      , "transformers"
      , "tuples"
      , "typelevel-prelude"
      , "unsafe-coerce"
      ]
      , repo = "ssh://git@ssh.bitbucket.juspay.net/fram/purescript-foreign-generic.git"
      , version = "v12.0.0"
    }
