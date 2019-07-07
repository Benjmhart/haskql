{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "hsql-pub"
, dependencies =
    [ "affjax"
    , "argonaut"
    , "argonaut-codecs"
    , "arrays"
    , "console"
    , "css"
    , "debug"
    , "effect"
    , "exitcodes"
    , "foreign"
    , "formatters"
    , "halogen"
    , "halogen-css"
    , "lists"
    , "maybe"
    , "node-process"
    , "now"
    , "optparse"
    , "profunctor-lenses"
    , "psci-support"
    , "refs"
    , "routing"
    , "routing-duplex"
    , "simple-json"
    , "slug"
    , "stringutils"
    ]
, packages =
    ./packages.dhall
}
