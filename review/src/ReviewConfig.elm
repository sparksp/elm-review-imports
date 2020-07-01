module ReviewConfig exposing (config)

import Documentation.ReadmeLinksPointToCurrentVersion
import NoAlways
import NoBooleanCase
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoForbiddenWords
import NoImportingEverything
import NoInconsistentAliases
import NoLeftPizza
import NoMissingTypeAnnotation
import NoModuleOnExposedNames
import NoRedundantConcat
import NoRedundantCons
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)
import UseCamelCase


config : List Rule
config =
    [ Documentation.ReadmeLinksPointToCurrentVersion.rule
    , NoAlways.rule
    , NoBooleanCase.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForDirectories
            [ -- Debug.toString is sometimes used in test failure messages.
              "tests/"
            ]
    , NoExposingEverything.rule
    , NoForbiddenWords.rule
        [ "- [ ]"
        , "TODO"
        ]
    , NoImportingEverything.rule []
    , NoInconsistentAliases.config
        [ ( "Review.Rule", "Rule" )
        ]
        |> NoInconsistentAliases.noMissingAliases
        |> NoInconsistentAliases.rule
    , NoLeftPizza.rule NoLeftPizza.Any
        |> Rule.ignoreErrorsForDirectories
            [ -- Test functions are traditionally built up using a left pizza.
              -- While we don't want them in our regular code, let's allow them
              -- just for tests.
              "tests/"
            ]
    , NoLeftPizza.rule NoLeftPizza.Redundant
        |> Rule.ignoreErrorsForDirectories
            [ -- Only check tests for redundant left pizza.
              "src/"
            ]
    , NoMissingTypeAnnotation.rule
    , NoModuleOnExposedNames.rule
    , NoRedundantConcat.rule
    , NoRedundantCons.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , UseCamelCase.rule UseCamelCase.default
    ]
