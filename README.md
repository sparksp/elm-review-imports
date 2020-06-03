# review-imports

![elm package](https://img.shields.io/elm-package/v/sparksp/elm-review-imports)
![elm-review 2.0](https://img.shields.io/badge/elm--review-2.0-%231293D8)
![elm 0.19](https://img.shields.io/badge/elm-0.19-%231293D8)
![Tests](https://github.com/sparksp/elm-review-imports/workflows/Tests/badge.svg)

An [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule to enforce consistent import aliases.

## Provided Rule

- [`NoInconsistentAliases`](https://package.elm-lang.org/packages/sparksp/elm-review-imports/1.0.0/NoUnusedPorts) - enforce consistent aliases.
- [`NoModuleOnExposedNames`](https://package.elm-lang.org/packages/sparksp/elm-review-imports/1.0.0/NoModuleOnExposedNames) - forbid module on exposed names.

## Example Configuration

```elm
import NoInconsistentAliases
import NoModuleOnExposedNames
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoInconsistentAliases.config
        [ ( "Html.Attributes", "Attr" )
        , ( "Json.Decode", "Decode" )
        , ( "Json.Encode", "Encode" )
        ]
        |> NoInconsistentAliases.detectAliases
        |> NoInconsistentAliases.noMissingAliases
        |> NoInconsistentAliases.rule
    , NoModuleOnExposedNames.rule
    ]
```

## Suggested Workflow

We recommend that you do not configure too many preferred aliases and instead use `NoInconsistentAliases.detectAliases` on your project, this will do a good job of highlighting inconsistent aliases. You should configure the preferred aliases to quickly fix any errant aliases using `elm-review --fix-all` - it's up to you then if you commit these or remove them again after.