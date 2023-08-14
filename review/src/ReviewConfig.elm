module ReviewConfig exposing (config)

import Docs.NoMissing exposing (exposedModules, onlyExposed)
import Docs.ReviewAtDocs
import Docs.ReviewLinksAndSections
import Docs.UpToDateReadmeLinks
import NoAlways
import NoDebug.Log
import NoDebug.TodoOrToString
import NoDuplicatePorts
import NoExposingEverything
import NoForbiddenWords
import NoImportingEverything
import NoInconsistentAliases
import NoMissingSubscriptionsCall
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoModuleOnExposedNames
import NoRecordAliasConstructor
import NoRecordAliasWithConstructor
import NoRecursiveUpdate
import NoSimpleLetBody
import NoUnmatchedUnit
import NoUnnecessaryTrailingUnderscore
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
import ReviewPipelineStyles
import ReviewPipelineStyles.Custom exposing (noSinglePiplinesWithSimpleInputs)
import ReviewPipelineStyles.Premade
    exposing
        ( noMultilineLeftPizza
        , noPipelinesWithConfusingNonCommutativeFunctions
        , noRepeatedParentheticalApplication
        , noSemanticallyInfixFunctionsInLeftPipelines
        , noSingleLineRightPizza
        )
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
    , NoMissingSubscriptionsCall.rule
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
    , NoMissingTypeExpose.rule
    , NoModuleOnExposedNames.rule
    , NoRecordAliasConstructor.rule
    , NoRecordAliasWithConstructor.rule
    , NoRecursiveUpdate.rule
    , NoSimpleLetBody.rule
    , NoUnmatchedUnit.rule
    , NoUnsafePorts.rule NoUnsafePorts.any
    , NoUnnecessaryTrailingUnderscore.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.defaults
        |> NoUnused.Exports.reportUnusedProductionExports
            { isProductionFile = \{ moduleName, filePath, isInSourceDirectories } ->
                isInSourceDirectories || List.member "Vendor" moduleName
            , exceptionsAre = []
            }
        |> NoUnused.Exports.toRule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnusedPorts.rule
    , NoUnused.Variables.rule
    , NoUselessSubscriptions.rule
    , ReviewPipelineStyles.rule <|
        List.concat
            [ noMultilineLeftPizza
            , noSingleLineRightPizza
            , noRepeatedParentheticalApplication
            , noPipelinesWithConfusingNonCommutativeFunctions
            , noSemanticallyInfixFunctionsInLeftPipelines
            , noSinglePiplinesWithSimpleInputs
            ]
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
