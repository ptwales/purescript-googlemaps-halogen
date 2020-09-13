let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200911-2/packages.dhall sha256:872c06349ed9c8210be43982dc6466c2ca7c5c441129826bcb9bf3672938f16e

let overrides = {=}
let additions =
    { googlemaps =
        { dependencies =
            [ "debug"
            , "foreign"
            , "functions"
            , "psci-support"
            , "read"
            , "spec-mocha"
            , "web-html"
            ]
        , repo = "https://github.com/ptwales/purescript-googlemaps.git"
        , version = "58ecf68c16a00447a049cd40df373b23d1165117"
        }
    , spec-mocha =
        { dependencies =
            [ "console"
            , "foldable-traversable"
            , "exceptions"
            , "spec"
            ]
        , repo =
            "https://github.com/owickstrom/purescript-spec-mocha.git"
        , version =
            "v4.0.0"
        }
    }
in  upstream // overrides // additions
