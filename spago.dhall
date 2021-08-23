{ name = "rss-client-proto"
, dependencies =
  [ "aff"
  , "console"
  , "const"
  , "effect"
  , "halogen"
  , "lists"
  , "maybe"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
