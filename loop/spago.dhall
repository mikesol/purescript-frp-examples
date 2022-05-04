{ name = "joyride"
, dependencies =
  [ "aff"
  , "arrays"
  , "behaviors"
  , "control"
  , "deku"
  , "effect"
  , "event"
  , "filterable"
  , "foldable-traversable"
  , "free"
  , "homogeneous"
  , "identity"
  , "maybe"
  , "newtype"
  , "numbers"
  , "parallel"
  , "prelude"
  , "tuples"
  , "wags"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
