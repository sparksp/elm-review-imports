# elm-review-imports

![elm package](https://img.shields.io/elm-package/v/sparksp/elm-review-imports)
![elm-review 2.3](https://img.shields.io/badge/elm--review-2.3-%231293D8)
![elm 0.19](https://img.shields.io/badge/elm-0.19-%231293D8)
![Tests](https://github.com/sparksp/elm-review-imports/workflows/Tests/badge.svg)

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to enforce consistent import aliases.


## Provided rules

- [`NoInconsistentAliases`](https://package.elm-lang.org/packages/sparksp/elm-review-imports/1.0.0/NoInconsistentAliases) - enforce consistent aliases.
- [`NoModuleOnExposedNames`](https://package.elm-lang.org/packages/sparksp/elm-review-imports/1.0.0/NoModuleOnExposedNames) - forbid module on exposed names.


## Configuration

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
        |> NoInconsistentAliases.noMissingAliases
        |> NoInconsistentAliases.rule
    , NoModuleOnExposedNames.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template sparksp/elm-review-imports/example
```
