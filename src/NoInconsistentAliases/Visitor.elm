module NoInconsistentAliases.Visitor exposing (rule)

import Dict
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import List.Nonempty as Nonempty
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
    Rule.newModuleRuleSchema "NoInconsistentAliases" Context.initial
        |> Rule.withImportVisitor (importVisitor options)
        |> NameVisitor.withNameVisitor moduleCallVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


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
        moduleAlias =
            maybeModuleAlias |> Maybe.withDefault moduleName |> Node.map formatModuleName
    in
    context |> Context.addModuleAlias (Node.value moduleName) (Node.value moduleAlias)


rememberBadAlias : Options -> Node ModuleName -> Maybe (Node ModuleName) -> Context.Module -> Context.Module
rememberBadAlias { lookupAliases, canMissAliases } (Node moduleNameRange moduleName) maybeModuleAlias context =
    case ( lookupAliases moduleName, maybeModuleAlias ) of
        ( Just expectedAliases, Just (Node moduleAliasRange moduleAlias) ) ->
            if Nonempty.all ((/=) (formatModuleName moduleAlias)) expectedAliases then
                let
                    badAlias =
                        BadAlias.new
                            { name = moduleAlias |> formatModuleName
                            , moduleName = moduleName
                            , expectedNames = expectedAliases
                            , range = moduleAliasRange
                            }
                in
                context |> Context.addBadAlias badAlias

            else
                context

        ( Just expectedAliases, Nothing ) ->
            if canMissAliases then
                context

            else
                let
                    missingAlias =
                        MissingAlias.new moduleName (Nonempty.head expectedAliases) moduleNameRange
                in
                context |> Context.addMissingAlias missingAlias

        ( Nothing, _ ) ->
            context


moduleCallVisitor : Node ( ModuleName, String ) -> Context.Module -> ( List (Error {}), Context.Module )
moduleCallVisitor node context =
    case Node.value node of
        ( moduleName, function ) ->
            ( [], Context.addModuleCall moduleName function (Node.range node) context )


finalEvaluation : Context.Module -> List (Error {})
finalEvaluation context =
    let
        lookupModuleNames =
            Context.lookupModuleNames context
    in
    Context.foldBadAliases (foldBadAliasError lookupModuleNames) [] context
        ++ Context.foldMissingAliases foldMissingAliasError [] context


foldBadAliasError : ModuleNameLookup -> BadAlias -> List (Error {}) -> List (Error {})
foldBadAliasError lookupModuleNames badAlias errors =
    let
        moduleName =
            badAlias |> BadAlias.mapModuleName identity

        expectedAliases =
            badAlias |> BadAlias.mapExpectedNames identity

        preferredAlias =
            Nonempty.head expectedAliases

        moduleClashes =
            Nonempty.foldl
                (\aliasName dict ->
                    case detectCollisions (lookupModuleNames aliasName) moduleName of
                        [] ->
                            dict

                        clashes ->
                            Dict.insert aliasName clashes dict
                )
                Dict.empty
                expectedAliases

        availableAliases =
            expectedAliases
                |> Nonempty.toList
                |> List.filter (\aliasName -> Dict.get aliasName moduleClashes == Nothing)

        expectedAlias =
            List.head availableAliases |> Maybe.withDefault preferredAlias
    in
    case ( Dict.toList moduleClashes, availableAliases ) of
        ( _, _ :: _ ) ->
            let
                badRange =
                    BadAlias.range badAlias

                fixes =
                    Fix.replaceRangeBy badRange expectedAlias
                        :: BadAlias.mapUses (fixModuleUse expectedAlias) badAlias
            in
            Rule.errorWithFix (incorrectAliasMessage expectedAlias badAlias) badRange fixes
                :: errors

        ( _ :: _, _ ) ->
            Rule.error (collisionAliasMessage expectedAlias badAlias) (BadAlias.range badAlias)
                :: errors

        ( [], [] ) ->
            errors


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


detectCollisions : List ModuleName -> ModuleName -> List ModuleName
detectCollisions collisionNames moduleName =
    List.filter ((/=) moduleName) collisionNames


incorrectAliasMessage : String -> BadAlias -> { message : String, details : List String }
incorrectAliasMessage expectedAlias badAlias =
    let
        badAliasName =
            BadAlias.mapName identity badAlias

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


collisionAliasMessage : String -> BadAlias -> { message : String, details : List String }
collisionAliasMessage expectedAlias badAlias =
    let
        badAliasName =
            BadAlias.mapName identity badAlias

        moduleName =
            BadAlias.mapModuleName formatModuleName badAlias
    in
    { message =
        "Incorrect alias `" ++ badAliasName ++ "` for module `" ++ moduleName ++ "`."
    , details =
        [ "This import does not use your preferred alias `" ++ expectedAlias ++ "` for `" ++ moduleName ++ "`."
        , "Your preferred alias has already been used by another module so you should review carefully whether to overload this alias or configure another."
        , "If you change this alias remember to change all references to the alias in this module too."
        ]
    }


missingAliasMessage : String -> MissingAlias -> { message : String, details : List String }
missingAliasMessage expectedAlias missingAlias =
    let
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
    Fix.replaceRangeBy (ModuleUse.range use) (ModuleUse.mapFunction (\name -> expectedAlias ++ "." ++ name) use)


formatModuleName : ModuleName -> String
formatModuleName moduleName =
    String.join "." moduleName


type alias ModuleNameLookup =
    String -> List ModuleName
