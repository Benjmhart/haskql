{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
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
    , "node-process"
    , "now"
    , "optparse"
    , "psci-support"
    , "routing"
    , "routing-duplex"
    , "simple-json"
    , "slug"
    ]
, packages =
    ./packages.dhall
}
