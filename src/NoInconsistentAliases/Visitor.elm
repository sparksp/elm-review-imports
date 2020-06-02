module NoInconsistentAliases.Visitor exposing (rule)

import Dict exposing (Dict)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import NoInconsistentAliases.BadAlias as BadAlias exposing (BadAlias)
import NoInconsistentAliases.Config exposing (Config)
import NoInconsistentAliases.Context as Context
import NoInconsistentAliases.MissingAlias as MissingAlias exposing (MissingAlias)
import NoInconsistentAliases.ModuleUse as ModuleUse exposing (ModuleUse)
import NoInconsistentAliases.Visitor.Options as Options exposing (Options)
import Review.Fix as Fix exposing (Fix)
import Review.Rule as Rule exposing (Error, Rule)
import Vendor.NameVisitor as NameVisitor


rule : Config -> Rule
rule config =
    let
        options : Options
        options =
            Options.fromConfig config
    in
    Rule.newProjectRuleSchema "NoInconsistentAliases" Context.initial
        |> Rule.withModuleVisitor (moduleVisitor options)
        |> Context.withModuleContext
        |> Rule.withFinalProjectEvaluation (finalProjectEvaluation options.lookupAlias)
        |> Rule.fromProjectRuleSchema


moduleVisitor :
    Options
    -> Rule.ModuleRuleSchema state Context.Module
    -> Rule.ModuleRuleSchema { state | hasAtLeastOneVisitor : () } Context.Module
moduleVisitor options schema =
    schema
        |> Rule.withImportVisitor (importVisitor options)
        |> NameVisitor.withNameVisitor moduleCallVisitor
        |> Rule.withFinalModuleEvaluation (finalModuleEvaluation options.lookupAlias)


importVisitor : Options -> Node Import -> Context.Module -> ( List (Error {}), Context.Module )
importVisitor options node context =
    let
        moduleNameNode =
            node |> Node.value |> .moduleName

        moduleName =
            moduleNameNode |> Node.value |> formatModuleName

        maybeModuleAlias =
            node |> Node.value |> .moduleAlias |> Maybe.map (Node.map formatModuleName)
    in
    ( []
    , context
        |> rememberModuleAlias (Node.map (\_ -> moduleName) moduleNameNode) maybeModuleAlias
        |> rememberBadAlias options moduleNameNode maybeModuleAlias
    )


rememberModuleAlias : Node String -> Maybe (Node String) -> Context.Module -> Context.Module
rememberModuleAlias moduleName maybeModuleAlias context =
    case maybeModuleAlias of
        Just moduleAlias ->
            context |> Context.addModuleAlias (Node.value moduleName) moduleAlias

        Nothing ->
            context |> Context.addModuleAlias (Node.value moduleName) moduleName


rememberBadAlias : Options -> Node ModuleName -> Maybe (Node String) -> Context.Module -> Context.Module
rememberBadAlias { lookupAlias, canMissAliases } moduleNameNode maybeModuleAlias context =
    let
        moduleName =
            Node.value moduleNameNode |> formatModuleName
    in
    case ( lookupAlias moduleName, maybeModuleAlias ) of
        ( Just expectedAlias, Just moduleAlias ) ->
            if expectedAlias /= (moduleAlias |> Node.value) then
                let
                    badAlias =
                        BadAlias.new (Node.value moduleAlias) moduleName expectedAlias (Node.range moduleAlias)
                in
                context |> Context.addBadAlias badAlias

            else
                context

        ( Just expectedAlias, Nothing ) ->
            if canMissAliases then
                context

            else
                let
                    missingAlias =
                        MissingAlias.new (Node.value moduleNameNode) expectedAlias (Node.range moduleNameNode)
                in
                context |> Context.addMissingAlias missingAlias

        ( Nothing, _ ) ->
            context


moduleCallVisitor : Node ( ModuleName, String ) -> Context.Module -> ( List (Error {}), Context.Module )
moduleCallVisitor node context =
    case Node.value node of
        ( moduleName, function ) ->
            ( [], Context.addModuleCall moduleName function (Node.range node) context )


finalProjectEvaluation : Options.AliasLookup -> Context.Project -> List (Error scope)
finalProjectEvaluation lookupAlias context =
    context
        |> Context.inconsistentModuleAliases
        |> Dict.filter (\moduleName _ -> lookupAlias moduleName == Nothing)
        |> reportInconsistentModuleAliases


reportInconsistentModuleAliases : Dict String (Dict String (List ( Rule.ModuleKey, Range ))) -> List (Error scope)
reportInconsistentModuleAliases modules =
    modules
        |> Dict.toList
        |> List.concatMap inconsistentModuleImportErrors


inconsistentModuleImportErrors : ( String, Dict String (List ( Rule.ModuleKey, Range )) ) -> List (Error scope)
inconsistentModuleImportErrors ( moduleName, aliases ) =
    let
        knownAliases =
            aliases |> Dict.keys
    in
    aliases
        |> Dict.toList
        |> List.concatMap (inconsistentModuleAliasErrors moduleName knownAliases)


inconsistentModuleAliasErrors : String -> List String -> ( String, List ( Rule.ModuleKey, Range ) ) -> List (Error scope)
inconsistentModuleAliasErrors moduleName knownAliases ( badAlias, imports ) =
    imports
        |> List.map (inconsistentModuleAliasError moduleName knownAliases badAlias)


inconsistentModuleAliasError : String -> List String -> String -> ( Rule.ModuleKey, Range ) -> Error scope
inconsistentModuleAliasError moduleName aliases badAlias ( moduleKey, range ) =
    Rule.errorForModule
        moduleKey
        { message = "Inconsistent alias `" ++ badAlias ++ "` for module `" ++ moduleName ++ "`."
        , details =
            [ "This module has been aliased differently across your project, you should pick one alias to be consistent everywhere.\n"
                ++ (aliases |> List.map (\line -> " * " ++ line) |> String.join "\n")
            ]
        }
        range


finalModuleEvaluation : Options.AliasLookup -> Context.Module -> List (Error {})
finalModuleEvaluation lookupAlias context =
    let
        lookupModuleName =
            Context.lookupModuleName context
    in
    Context.foldBadAliases (foldBadAliasError lookupAlias lookupModuleName) [] context
        ++ Context.foldMissingAliases foldMissingAliasError [] context


foldBadAliasError : Options.AliasLookup -> ModuleNameLookup -> BadAlias -> List (Error {}) -> List (Error {})
foldBadAliasError lookupAlias lookupModuleName badAlias errors =
    let
        moduleName =
            badAlias |> BadAlias.mapModuleName identity

        expectedAlias =
            badAlias |> BadAlias.mapExpectedName identity

        moduleClash =
            detectCollision (lookupModuleName expectedAlias) moduleName

        aliasClash =
            moduleClash |> Maybe.andThen lookupAlias
    in
    case ( aliasClash, moduleClash ) of
        ( Just _, _ ) ->
            errors

        ( Nothing, Just collisionName ) ->
            Rule.error (collisionAliasMessage collisionName expectedAlias badAlias) (BadAlias.range badAlias)
                :: errors

        ( Nothing, Nothing ) ->
            let
                badRange =
                    BadAlias.range badAlias

                fixes =
                    Fix.replaceRangeBy badRange expectedAlias
                        :: BadAlias.mapUses (fixModuleUse expectedAlias) badAlias
            in
            Rule.errorWithFix (incorrectAliasMessage expectedAlias badAlias) badRange fixes
                :: errors


foldMissingAliasError : MissingAlias -> List (Error {}) -> List (Error {})
foldMissingAliasError missingAlias errors =
    if MissingAlias.hasUses missingAlias then
        let
            expectedAlias =
                missingAlias |> MissingAlias.mapExpectedName identity

            badRange =
                MissingAlias.range missingAlias

            fixes =
                Fix.insertAt badRange.end (" as " ++ expectedAlias)
                    :: MissingAlias.mapUses (fixModuleUse expectedAlias) missingAlias
        in
        Rule.errorWithFix (missingAliasMessage expectedAlias missingAlias) badRange fixes
            :: errors

    else
        errors


detectCollision : Maybe String -> String -> Maybe String
detectCollision maybeCollisionName moduleName =
    maybeCollisionName
        |> Maybe.andThen
            (\collisionName ->
                if collisionName == moduleName then
                    Nothing

                else
                    Just collisionName
            )


incorrectAliasMessage : String -> BadAlias -> { message : String, details : List String }
incorrectAliasMessage expectedAlias badAlias =
    let
        moduleName =
            BadAlias.mapModuleName quote badAlias
    in
    { message =
        "Incorrect alias " ++ BadAlias.mapName quote badAlias ++ " for module " ++ moduleName ++ "."
    , details =
        [ "This import does not use your preferred alias " ++ quote expectedAlias ++ " for " ++ moduleName ++ "."
        , "You should update the alias to be consistent with the rest of the project. "
            ++ "Remember to change all references to the alias in this module too."
        ]
    }


collisionAliasMessage : String -> String -> BadAlias -> { message : String, details : List String }
collisionAliasMessage collisionName expectedAlias badAlias =
    let
        moduleName =
            BadAlias.mapModuleName quote badAlias
    in
    { message =
        "Incorrect alias " ++ BadAlias.mapName quote badAlias ++ " for module " ++ moduleName ++ "."
    , details =
        [ "This import does not use your preferred alias " ++ quote expectedAlias ++ " for " ++ moduleName ++ "."
        , "Your preferred alias has already been taken by " ++ quote collisionName ++ "."
        , "You should change the alias for both modules to be consistent with the rest of the project. "
            ++ "Remember to change all references to the alias in this module too."
        ]
    }


missingAliasMessage : String -> MissingAlias -> { message : String, details : List String }
missingAliasMessage expectedAlias missingAlias =
    let
        moduleName =
            MissingAlias.mapModuleName (formatModuleName >> quote) missingAlias
    in
    { message =
        "Expected alias " ++ quote expectedAlias ++ " missing for module " ++ moduleName ++ "."
    , details =
        [ "This import does not use your preferred alias " ++ quote expectedAlias ++ " for " ++ moduleName ++ "."
        , "You should update the alias to be consistent with the rest of the project. "
            ++ "Remember to change all references to the alias in this module too."
        ]
    }


fixModuleUse : String -> ModuleUse -> Fix
fixModuleUse expectedAlias use =
    Fix.replaceRangeBy (ModuleUse.range use) (ModuleUse.mapFunction (\name -> expectedAlias ++ "." ++ name) use)


formatModuleName : ModuleName -> String
formatModuleName moduleName =
    String.join "." moduleName


quote : String -> String
quote string =
    "`" ++ string ++ "`"


type alias ModuleNameLookup =
    String -> Maybe String
