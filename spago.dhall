{ name = "presto"
, dependencies = 
    [ "aff"
    , "avar"
    , "datetime"
    , "effect"
    , "either"
    , "exceptions"
    , "exists"
    , "foldable-traversable"
    , "foreign"
    , "foreign-generic"
    , "foreign-object"
    , "free"
    , "identity"
    , "maybe"
    , "newtype"
    , "parallel"
    , "prelude"
    , "record"
    , "transformers"
    , "tuples"
    , "unsafe-coerce" 
    , "arrays"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
