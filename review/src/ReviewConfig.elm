module ReviewConfig exposing (config)

import Docs.NoMissing exposing (exposedModules, onlyExposed)
import Docs.ReviewAtDocs
import Docs.ReviewLinksAndSections
import Docs.UpToDateReadmeLinks
import NoAlways
import NoBooleanCase
import NoDebug.Log
import NoDebug.TodoOrToString
import NoDuplicatePorts
import NoExposingEverything
import NoForbiddenWords
import NoImportingEverything
import NoInconsistentAliases
import NoLeftPizza
import NoMissingSubscriptionsCall
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoModuleOnExposedNames
import NoRecordAliasConstructor
import NoRecordAliasWithConstructor
import NoRecursiveUpdate
import NoRedundantConcat
import NoRedundantCons
import NoUnsafePorts
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import NoUnusedPorts
import NoUselessSubscriptions
import Review.Rule as Rule exposing (Rule)
import Simplify
import UseCamelCase
import Vendor.NoFullyAppliedPrefixOperator as NoFullyAppliedPrefixOperator


config : List Rule
config =
    [ Docs.NoMissing.rule
        { document = onlyExposed
        , from = exposedModules
        }
    , Docs.ReviewLinksAndSections.rule
    , Docs.ReviewAtDocs.rule
    , Docs.UpToDateReadmeLinks.rule
    , NoAlways.rule
    , NoBooleanCase.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForDirectories
            [ -- Debug.toString is sometimes used in test failure messages.
              "tests/"
            ]
    , NoDuplicatePorts.rule
    , NoExposingEverything.rule
    , NoForbiddenWords.rule
        [ "- [ ]"
        , "REPLACEME"
        , "TODO"
        ]
    , NoFullyAppliedPrefixOperator.rule
    , NoImportingEverything.rule []
    , NoInconsistentAliases.config
        [ ( "Html.Attributes", "Attr" )
        , ( "Json.Decode", "Decode" )
        , ( "Json.Encode", "Encode" )
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
    , NoMissingSubscriptionsCall.rule
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
    , NoMissingTypeExpose.rule
    , NoModuleOnExposedNames.rule
    , NoRecordAliasConstructor.rule
    , NoRecordAliasWithConstructor.rule
    , NoRecursiveUpdate.rule
    , NoRedundantConcat.rule
    , NoRedundantCons.rule
    , NoUnsafePorts.rule NoUnsafePorts.any
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnusedPorts.rule
    , NoUnused.Variables.rule
    , NoUselessSubscriptions.rule
    , Simplify.rule Simplify.defaults
    , UseCamelCase.rule UseCamelCase.default
    ]
        |> List.map
            (Rule.ignoreErrorsForFiles
                [ "src/Html/Tailwind.elm"
                , "src/Svg/Tailwind.elm"
                ]
            )
        |> List.map
            (Rule.ignoreErrorsForDirectories
                [ "gen"
                ]
            )
