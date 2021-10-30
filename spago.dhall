{ name = "sorss-client"
, dependencies =
  [ "aff"
  , "affjax"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "halogen"
  , "halogen-hooks"
  , "halogen-store"
  , "http-methods"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "remotedata"
  , "routing"
  , "routing-duplex"
  , "safe-coerce"
  , "simple-json"
  , "tuples"
  , "typelevel-prelude"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}