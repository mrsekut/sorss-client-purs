{ name = "rss-client-proto"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "either"
  , "halogen"
  , "halogen-store"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "routing"
  , "routing-duplex"
  , "safe-coerce"
  , "typelevel-prelude"
  , "web-events"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
