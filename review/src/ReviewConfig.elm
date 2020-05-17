module ReviewConfig exposing (config)

import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoInconsistentAliases
import NoModuleOnExposedNames
import NoMissingTypeAnnotation
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Variables
import Review.Rule exposing (Rule)
import UseCamelCase


config : List Rule
config =
    [ NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoExposingEverything.rule
    , NoImportingEverything.rule []
    , NoInconsistentAliases.config
        [ ( "Review.Rule", "Rule" )
        ]
        |> NoInconsistentAliases.rule
    , NoMissingTypeAnnotation.rule
    , NoModuleOnExposedNames.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Variables.rule
    , UseCamelCase.rule UseCamelCase.default
    ]
