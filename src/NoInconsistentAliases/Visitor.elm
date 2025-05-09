module NoInconsistentAliases.Visitor exposing (rule)

import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import NoInconsistentAliases.BadAlias as BadAlias exposing (BadAlias)
import NoInconsistentAliases.Config as Config exposing (Config)
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
    Rule.newProjectRuleSchema "NoInconsistentAliases" Context.initialProject
        |> Rule.withModuleVisitor
            (\visitor ->
                visitor
                    |> Rule.withImportVisitor (importVisitor options)
                    |> NameVisitor.withNameVisitor moduleCallVisitor
                    |> Rule.withFinalModuleEvaluation (finalEvaluation options.lookupAlias)
                    |> Rule.providesFixesForModuleRule
            )
        |> Rule.withModuleContextUsingContextCreator (Context.converter options)
        |> (case options.discoverAliases of
                Config.DoNotDiscover ->
                    identity

                Config.DiscoverAndShowSummary ->
                    Rule.withFinalProjectEvaluation discoverAndShowSummary >> Rule.providesFixesForProjectRule

                Config.DiscoverAndShowIndividual ->
                    Rule.withFinalProjectEvaluation discoverAndShowIndividual
           )
        |> Rule.fromProjectRuleSchema


discoverAndShowSummary : Context.Project -> List (Error { useErrorForModule : () })
discoverAndShowSummary project =
    project
        |> Context.getAliasesGroups
        |> List.concatMap mismatchedAliasToErrorSummary


mismatchedAliasToErrorSummary :
    { from : ModuleName
    , tos :
        List
            ( String
            , List
                { inModule : List String
                , aliasRange : Range
                , key : Rule.ModuleKey
                }
            )
    }
    -> List (Error { useErrorForModule : () })
mismatchedAliasToErrorSummary { from, tos } =
    let
        fromName : String
        fromName =
            formatModuleName from
    in
    case filterAndSortTos fromName tos of
        [] ->
            []

        [ _ ] ->
            []

        sorted ->
            [ Rule.globalError
                { message = "Mismatched aliases for `" ++ fromName ++ "`"
                , details =
                    List.map
                        (\( to, modules ) ->
                            let
                                count : Int
                                count =
                                    List.length modules

                                common : String
                                common =
                                    " - `" ++ to ++ "` is used " ++ String.fromInt count ++ " times"
                            in
                            if count < 20 then
                                common
                                    ++ " in "
                                    ++ String.join ", "
                                        (List.map (\{ inModule } -> "`" ++ formatModuleName inModule ++ "`") modules)
                                    ++ "."

                            else
                                common ++ "."
                        )
                        sorted
                }
            ]


filterAndSortTos :
    String
    ->
        List
            ( String
            , List
                { inModule : List String
                , aliasRange : Range
                , key : Rule.ModuleKey
                }
            )
    ->
        List
            ( String
            , List
                { inModule : List String
                , aliasRange : Range
                , key : Rule.ModuleKey
                }
            )
filterAndSortTos fromName tos =
    tos
        |> List.sortBy
            (\( to, modules ) ->
                ( if fromName == to then
                    1

                  else
                    0
                , -(List.length modules)
                )
            )


discoverAndShowIndividual : Context.Project -> List (Error { useErrorForModule : () })
discoverAndShowIndividual project =
    project
        |> Context.getAliasesGroups
        |> List.concatMap mismatchedAliasToErrorIndividual


mismatchedAliasToErrorIndividual :
    { from : ModuleName
    , tos :
        List
            ( String
            , List
                { inModule : List String
                , aliasRange : Range
                , key : Rule.ModuleKey
                }
            )
    }
    -> List (Error { useErrorForModule : () })
mismatchedAliasToErrorIndividual { from, tos } =
    let
        fromName : String
        fromName =
            formatModuleName from
    in
    case filterAndSortTos fromName tos of
        [] ->
            []

        [ _ ] ->
            []

        ( correct, _ ) :: wrongs ->
            List.concatMap
                (\( to, modules ) ->
                    List.map
                        (\{ key, aliasRange } ->
                            Rule.errorForModule key
                                { message =
                                    "Incorrect alias `" ++ to ++ "` for module `" ++ fromName ++ "`."
                                , details =
                                    [ "This import does not use your preferred alias `" ++ correct ++ "` for `" ++ fromName ++ "`."
                                    , "You should update the alias to be consistent with the rest of the project. Remember to change all references to the alias in this module too."
                                    ]
                                }
                                aliasRange
                        )
                        modules
                )
                wrongs


importVisitor : Options -> Node Import -> Context.Module -> ( List (Error {}), Context.Module )
importVisitor options (Node _ { moduleName, moduleAlias }) context =
    ( []
    , context
        |> rememberModuleAlias moduleName moduleAlias
        |> rememberBadAlias options moduleName moduleAlias
    )


rememberModuleAlias : Node ModuleName -> Maybe (Node ModuleName) -> Context.Module -> Context.Module
rememberModuleAlias moduleName maybeModuleAlias context =
    let
        moduleAlias : Node String
        moduleAlias =
            maybeModuleAlias
                |> Maybe.withDefault moduleName
                |> Node.map formatModuleName
    in
    Context.addModuleAlias (Node.value moduleName) moduleAlias context


rememberBadAlias : Options -> Node ModuleName -> Maybe (Node ModuleName) -> Context.Module -> Context.Module
rememberBadAlias { lookupAlias, canMissAliases } (Node moduleNameRange moduleName) maybeModuleAlias context =
    case ( lookupAlias moduleName, maybeModuleAlias ) of
        ( Just expectedAlias, Just (Node moduleAliasRange moduleAlias) ) ->
            if [ expectedAlias ] /= moduleAlias then
                let
                    badAlias : BadAlias
                    badAlias =
                        BadAlias.new
                            { name = formatModuleName moduleAlias
                            , moduleName = moduleName
                            , expectedName = expectedAlias
                            , range = moduleAliasRange
                            }
                in
                Context.addBadAlias badAlias context

            else
                context

        ( Just expectedAlias, Nothing ) ->
            if canMissAliases then
                context

            else
                let
                    missingAlias : MissingAlias
                    missingAlias =
                        MissingAlias.new moduleName expectedAlias moduleNameRange
                in
                Context.addMissingAlias missingAlias context

        ( Nothing, _ ) ->
            context


moduleCallVisitor : Node ( ModuleName, String ) -> Context.Module -> ( List (Error {}), Context.Module )
moduleCallVisitor node context =
    let
        ( moduleName, function ) =
            Node.value node
    in
    ( [], Context.addModuleCall moduleName function (Node.range node) context )


finalEvaluation : Options.AliasLookup -> Context.Module -> List (Error {})
finalEvaluation lookupAlias context =
    let
        lookupModuleName : String -> Maybe ModuleName
        lookupModuleName =
            Context.lookupModuleName context
    in
    Context.foldBadAliases (foldBadAliasError lookupAlias lookupModuleName) [] context
        ++ Context.foldMissingAliases foldMissingAliasError [] context


foldBadAliasError : Options.AliasLookup -> ModuleNameLookup -> BadAlias -> List (Error {}) -> List (Error {})
foldBadAliasError lookupAlias lookupModuleName badAlias errors =
    let
        moduleName : ModuleName
        moduleName =
            BadAlias.mapModuleName identity badAlias

        expectedAlias : String
        expectedAlias =
            BadAlias.mapExpectedName identity badAlias

        moduleClash : Maybe ModuleName
        moduleClash =
            detectCollision (lookupModuleName expectedAlias) moduleName

        aliasClash : Maybe String
        aliasClash =
            Maybe.andThen lookupAlias moduleClash
    in
    case ( aliasClash, moduleClash ) of
        ( Just _, _ ) ->
            errors

        ( Nothing, Just collisionName ) ->
            Rule.error (collisionAliasMessage collisionName expectedAlias badAlias) (BadAlias.range badAlias)
                :: errors

        ( Nothing, Nothing ) ->
            let
                badRange : Range
                badRange =
                    BadAlias.range badAlias

                fixes : List Fix
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
            expectedAlias : String
            expectedAlias =
                MissingAlias.mapExpectedName identity missingAlias

            badRange : Range
            badRange =
                MissingAlias.range missingAlias

            fixes : List Fix
            fixes =
                Fix.insertAt badRange.end (" as " ++ expectedAlias)
                    :: MissingAlias.mapUses (fixModuleUse expectedAlias) missingAlias
        in
        Rule.errorWithFix (missingAliasMessage expectedAlias missingAlias) badRange fixes
            :: errors

    else
        errors


detectCollision : Maybe ModuleName -> ModuleName -> Maybe ModuleName
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
        badAliasName : String
        badAliasName =
            BadAlias.mapName identity badAlias

        moduleName : String
        moduleName =
            BadAlias.mapModuleName formatModuleName badAlias
    in
    { message =
        "Incorrect alias `" ++ badAliasName ++ "` for module `" ++ moduleName ++ "`."
    , details =
        [ "This import does not use your preferred alias `" ++ expectedAlias ++ "` for `" ++ moduleName ++ "`."
        , "You should update the alias to be consistent with the rest of the project. Remember to change all references to the alias in this module too."
        ]
    }


collisionAliasMessage : ModuleName -> String -> BadAlias -> { message : String, details : List String }
collisionAliasMessage collisionName expectedAlias badAlias =
    let
        badAliasName : String
        badAliasName =
            BadAlias.mapName identity badAlias

        moduleName : String
        moduleName =
            BadAlias.mapModuleName formatModuleName badAlias
    in
    { message =
        "Incorrect alias `" ++ badAliasName ++ "` for module `" ++ moduleName ++ "`."
    , details =
        [ "This import does not use your preferred alias `" ++ expectedAlias ++ "` for `" ++ moduleName ++ "`."
        , "Your preferred alias has already been taken by `" ++ formatModuleName collisionName ++ "`."
        , "You should change the alias for both modules to be consistent with the rest of the project. Remember to change all references to the alias in this module too."
        ]
    }


missingAliasMessage : String -> MissingAlias -> { message : String, details : List String }
missingAliasMessage expectedAlias missingAlias =
    let
        moduleName : String
        moduleName =
            MissingAlias.mapModuleName formatModuleName missingAlias
    in
    { message =
        "Expected alias `" ++ expectedAlias ++ "` missing for module `" ++ moduleName ++ "`."
    , details =
        [ "This import does not use your preferred alias `" ++ expectedAlias ++ "` for `" ++ moduleName ++ "`."
        , "You should update the alias to be consistent with the rest of the project. Remember to change all references to the alias in this module too."
        ]
    }


fixModuleUse : String -> ModuleUse -> Fix
fixModuleUse expectedAlias use =
    ModuleUse.mapFunction (\name -> expectedAlias ++ "." ++ name) use
        |> Fix.replaceRangeBy (ModuleUse.range use)


formatModuleName : ModuleName -> String
formatModuleName moduleName =
    String.join "." moduleName


type alias ModuleNameLookup =
    String -> Maybe ModuleName
