{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "functions"
  , "halogen"
  , "halogen-store"
  , "maybe"
  , "newtype"
  , "numbers"
  , "ordered-collections"
  , "prelude"
  , "safe-coerce"
  , "spec"
  , "spec-node"
  , "strings"
  , "tuples"
  , "typelevel"
  , "unfoldable"
  , "unicode"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
