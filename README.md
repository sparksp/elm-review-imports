# review-imports

![elm package](https://img.shields.io/elm-package/v/sparksp/elm-review-imports)
![elm-review 2.0](https://img.shields.io/badge/elm--review-2.0-%231293D8)
![elm 0.19](https://img.shields.io/badge/elm-0.19-%231293D8)
![Tests](https://github.com/sparksp/elm-review-imports/workflows/Tests/badge.svg)

An [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule to enforce consistent import aliases.

## Provided Rule

- [`NoInconsistentAliases`](https://package.elm-lang.org/packages/sparksp/elm-review-imports/latest/NoUnusedPorts) - enforce consistent aliases.

## Example Configuration

```elm
import NoInconsistentAliases
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoInconsistentAliases.config
        [ ( "Html.Attributes", "Attr" )
        , ( "Json.Decode", "Decode" )
        , ( "Json.Encode", "Encode" )
        ]
        |> NoInconsistentAliases.noMissingAliases
        |> NoInconsistentAliases.rule
    ]
```