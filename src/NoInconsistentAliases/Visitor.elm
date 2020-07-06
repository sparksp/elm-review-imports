module NoInconsistentAliases.Visitor exposing (rule)

import Dict exposing (Dict)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import List.Nonempty as Nonempty exposing (Nonempty)
import NoInconsistentAliases.Config as Config exposing (Config)
import Review.Fix as Fix exposing (Fix)
import Review.Rule as Rule exposing (Rule)
import Set exposing (Set)
import Vendor.NameVisitor as NameVisitor


rule : Config -> Rule
rule config =
    Rule.newModuleRuleSchema "NoInconsistentAliases" initialContext
        |> Rule.withImportVisitor importVisitor
        |> NameVisitor.withNameVisitor moduleCallVisitor
        |> Rule.withFinalModuleEvaluation
            (finalEvaluation
                { aliases = Config.listAliases config
                , canMissAliases = Config.canMissAliases config
                }
            )
        |> Rule.fromModuleRuleSchema


importVisitor : Node Import -> Context -> ( List (Rule.Error nothing), Context )
importVisitor (Node _ { moduleName, moduleAlias }) context =
    ( []
    , rememberImport moduleName moduleAlias context
    )


moduleCallVisitor : Node ( ModuleName, String ) -> Context -> ( List (Rule.Error nothing), Context )
moduleCallVisitor (Node range ( moduleName, function )) context =
    ( []
    , rememberModuleCall moduleName function range context
    )


finalEvaluation : { aliases : List ( ModuleName, Nonempty String ), canMissAliases : Bool } -> Context -> List (Rule.Error {})
finalEvaluation { aliases, canMissAliases } context =
    List.foldl (foldAliases canMissAliases context) ( Set.empty, [] ) aliases
        |> Tuple.second


foldAliases : Bool -> Context -> ( ModuleName, Nonempty String ) -> ( Set String, List (Rule.Error {}) ) -> ( Set String, List (Rule.Error {}) )
foldAliases canMissAliases context ( moduleName, aliases ) ( pastAliases, errors ) =
    ( Nonempty.foldl Set.insert pastAliases aliases
    , checkModuleImport canMissAliases context pastAliases moduleName aliases errors
    )


checkModuleImport : Bool -> Context -> Set String -> ModuleName -> Nonempty String -> List (Rule.Error {}) -> List (Rule.Error {})
checkModuleImport canMissAliases context pastAliases moduleName aliases errors =
    case getModuleAlias moduleName context of
        Nothing ->
            errors

        Just ( range, Nothing ) ->
            if canMissAliases then
                errors

            else
                checkForMissingAlias context pastAliases moduleName aliases range errors

        Just ( _, Just aliasNode ) ->
            checkForBadAlias context pastAliases moduleName aliases aliasNode errors


checkForMissingAlias : Context -> Set String -> ModuleName -> Nonempty String -> Range -> List (Rule.Error {}) -> List (Rule.Error {})
checkForMissingAlias context pastAliases moduleName aliases range errors =
    case getModuleCalls moduleName context of
        [] ->
            errors

        moduleCalls ->
            case firstAvailableAlias pastAliases aliases of
                Just expectedAlias ->
                    let
                        fixes =
                            Fix.insertAt range.end (" as " ++ expectedAlias)
                                :: List.map (fixModuleUse expectedAlias) moduleCalls
                    in
                    Rule.errorWithFix (missingAliasError expectedAlias moduleName) range fixes
                        :: errors

                Nothing ->
                    Rule.error (missingAliasCollisionError (Nonempty.last aliases) moduleName) range
                        :: errors


checkForBadAlias : Context -> Set String -> ModuleName -> Nonempty String -> Node String -> List (Rule.Error {}) -> List (Rule.Error {})
checkForBadAlias context pastAliases moduleName aliases (Node range aliasName) errors =
    case firstAvailableAlias pastAliases aliases of
        Just expectedAlias ->
            if expectedAlias /= aliasName then
                if
                    hasCollision context moduleName expectedAlias
                        || hasCollision context moduleName aliasName
                then
                    Rule.error (collisionAliasError expectedAlias aliasName moduleName) range
                        :: errors

                else
                    let
                        fixes =
                            Fix.replaceRangeBy range expectedAlias
                                :: List.map (fixModuleUse expectedAlias) (getModuleCalls [ aliasName ] context)
                    in
                    Rule.errorWithFix (incorrectAliasError expectedAlias aliasName moduleName) range fixes
                        :: errors

            else
                errors

        Nothing ->
            let
                expectedAlias =
                    Nonempty.last aliases
            in
            if expectedAlias /= aliasName then
                Rule.error (collisionAliasError expectedAlias aliasName moduleName) range
                    :: errors

            else
                errors


hasCollision : Context -> ModuleName -> String -> Bool
hasCollision context moduleName expectedName =
    case findModulesByImport expectedName context of
        [] ->
            False

        collisionModuleNames ->
            List.any ((/=) moduleName) collisionModuleNames


firstAvailableAlias : Set String -> Nonempty String -> Maybe String
firstAvailableAlias pastAliases aliases =
    Nonempty.foldl (firstAvailableAliasHelp pastAliases) Nothing aliases


firstAvailableAliasHelp : Set String -> String -> Maybe String -> Maybe String
firstAvailableAliasHelp pastAliases alias found =
    case found of
        Just _ ->
            found

        Nothing ->
            if Set.member alias pastAliases then
                Nothing

            else
                Just alias


fixModuleUse : String -> Node String -> Fix
fixModuleUse expectedAlias (Node range function) =
    Fix.replaceRangeBy range (expectedAlias ++ "." ++ function)


incorrectAliasError : String -> String -> ModuleName -> { message : String, details : List String }
incorrectAliasError expectedAlias badAliasName moduleName =
    let
        moduleNameString =
            formatModuleName moduleName
    in
    { message = incorrectAliasMessage badAliasName moduleNameString
    , details = incorrectAliasDetails expectedAlias moduleNameString
    }


collisionAliasError : String -> String -> ModuleName -> { message : String, details : List String }
collisionAliasError expectedAlias badAliasName moduleName =
    let
        moduleNameString =
            formatModuleName moduleName
    in
    { message = incorrectAliasMessage badAliasName moduleNameString
    , details = collisionAliasDetails expectedAlias moduleNameString
    }


incorrectAliasMessage : String -> String -> String
incorrectAliasMessage badAliasName moduleName =
    "Incorrect alias `" ++ badAliasName ++ "` for module `" ++ moduleName ++ "`."


missingAliasError : String -> ModuleName -> { message : String, details : List String }
missingAliasError expectedAlias moduleName =
    let
        moduleNameString =
            formatModuleName moduleName
    in
    { message = expectedAliasMessage expectedAlias moduleNameString
    , details = incorrectAliasDetails expectedAlias moduleNameString
    }


missingAliasCollisionError : String -> ModuleName -> { message : String, details : List String }
missingAliasCollisionError expectedAlias moduleName =
    let
        moduleNameString =
            formatModuleName moduleName
    in
    { message = expectedAliasMessage expectedAlias moduleNameString
    , details = collisionAliasDetails expectedAlias moduleNameString
    }


expectedAliasMessage : String -> String -> String
expectedAliasMessage expectedAlias moduleName =
    "Expected alias `" ++ expectedAlias ++ "` missing for module `" ++ moduleName ++ "`."


incorrectAliasDetails : String -> String -> List String
incorrectAliasDetails expectedAlias moduleName =
    [ "This import does not use your preferred alias `" ++ expectedAlias ++ "` for `" ++ moduleName ++ "`."
    , "You should update the alias to be consistent with the rest of the project. Remember to change all references to the alias in this module too."
    ]


collisionAliasDetails : String -> String -> List String
collisionAliasDetails expectedAlias moduleName =
    [ "This import does not use your preferred alias `" ++ expectedAlias ++ "` for `" ++ moduleName ++ "`."
    , "Your preferred alias has already been used by another module so you should review carefully whether to overload this alias or configure another."
    , "If you change this alias remember to change all references to the alias in this module too."
    ]



--- CONTEXT


type alias Context =
    { imports : Dict ModuleName ( Range, Maybe (Node String) )
    , moduleCalls : Dict ModuleName (List (Node String))
    }


initialContext : Context
initialContext =
    { imports = Dict.empty
    , moduleCalls = Dict.empty
    }


rememberImport : Node ModuleName -> Maybe (Node ModuleName) -> Context -> Context
rememberImport (Node range moduleName) maybeModuleAlias context =
    { context | imports = Dict.insert moduleName ( range, formatAliasName maybeModuleAlias ) context.imports }


rememberModuleCall : ModuleName -> String -> Range -> Context -> Context
rememberModuleCall moduleName function range context =
    { context
        | moduleCalls = Dict.update moduleName (addModuleCallUpdater (Node range function)) context.moduleCalls
    }


getModuleAlias : ModuleName -> Context -> Maybe ( Range, Maybe (Node String) )
getModuleAlias moduleName { imports } =
    Dict.get moduleName imports


getModuleCalls : ModuleName -> Context -> List (Node String)
getModuleCalls moduleName { moduleCalls } =
    Dict.get moduleName moduleCalls
        |> Maybe.withDefault []


findModulesByImport : String -> Context -> List ModuleName
findModulesByImport aliasName { imports } =
    Dict.foldl (findModuleByAliasHelp aliasName) [] imports


addModuleCallUpdater : Node String -> Maybe (List (Node String)) -> Maybe (List (Node String))
addModuleCallUpdater moduleCall maybeModuleCalls =
    Just (moduleCall :: Maybe.withDefault [] maybeModuleCalls)


findModuleByAliasHelp : String -> ModuleName -> ( Range, Maybe (Node String) ) -> List ModuleName -> List ModuleName
findModuleByAliasHelp searchAlias moduleName ( _, maybeModuleAlias ) acc =
    case maybeModuleAlias of
        Nothing ->
            if formatModuleName moduleName == searchAlias then
                moduleName :: acc

            else
                acc

        Just (Node _ moduleAlias) ->
            if moduleAlias == searchAlias then
                moduleName :: acc

            else
                acc


formatAliasName : Maybe (Node ModuleName) -> Maybe (Node String)
formatAliasName maybeAliasNode =
    Maybe.map (Node.map formatModuleName) maybeAliasNode


formatModuleName : ModuleName -> String
formatModuleName =
    String.join "."
