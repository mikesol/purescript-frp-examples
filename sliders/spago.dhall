{ name = "joyride"
, dependencies =
  [ "arrays"
  , "control"
  , "deku"
  , "effect"
  , "event"
  , "integers"
  , "maybe"
  , "numbers"
  , "prelude"
  , "tuples"
  , "typelevel"
  , "wags"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
