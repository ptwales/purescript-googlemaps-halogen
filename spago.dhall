{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-googlemaps"
, dependencies =
    [ "console"
    , "effect"
    , "googlemaps"
    , "halogen"
    , "psci-support"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
