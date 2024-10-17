module NoInconsistentAliases.Context exposing
    ( Module, Project, initialProject
    , addModuleAlias, lookupModuleName
    , addMissingAlias, foldMissingAliases
    , addBadAlias, foldBadAliases
    , addModuleCall
    , converter
    , getAliasesGroups
    )

{-|

@docs Module, Project, initialProject
@docs addModuleAlias, lookupModuleName
@docs addMissingAlias, foldMissingAliases
@docs addBadAlias, foldBadAliases
@docs addModuleCall
@docs converter
@docs getAliasesGroups

-}

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import NoInconsistentAliases.BadAlias exposing (BadAlias)
import NoInconsistentAliases.BadAliasSet as BadAliasSet exposing (BadAliasSet)
import NoInconsistentAliases.MissingAlias exposing (MissingAlias)
import NoInconsistentAliases.MissingAliasSet as MissingAliasSet exposing (MissingAliasSet)
import NoInconsistentAliases.ModuleUse as ModuleUse exposing (ModuleUse)
import NoInconsistentAliases.Visitor.Options exposing (Options)
import Review.Rule as Rule


type Project
    = Project
        (Dict
            ModuleName
            { key : Rule.ModuleKey
            , aliases :
                Dict
                    ModuleName
                    { to : String
                    , aliasRange : Range
                    }
            }
        )


initialProject : Project
initialProject =
    Project Dict.empty


type Module
    = Module
        { aliases : Dict String { from : ModuleName, aliasRange : Range }
        , badAliases : BadAliasSet
        , missingAliases : MissingAliasSet
        }


initialModule : Module
initialModule =
    Module
        { aliases = Dict.empty
        , badAliases = BadAliasSet.empty
        , missingAliases = MissingAliasSet.empty
        }


addModuleAlias : ModuleName -> Node String -> Module -> Module
addModuleAlias moduleName (Node aliasRange moduleAlias) (Module context) =
    Module { context | aliases = Dict.insert moduleAlias { from = moduleName, aliasRange = aliasRange } context.aliases }


addBadAlias : BadAlias -> Module -> Module
addBadAlias badAlias (Module context) =
    Module { context | badAliases = BadAliasSet.insert badAlias context.badAliases }


addMissingAlias : MissingAlias -> Module -> Module
addMissingAlias missingAlias (Module context) =
    Module { context | missingAliases = MissingAliasSet.insert missingAlias context.missingAliases }


addModuleCall : ModuleName -> String -> Range -> Module -> Module
addModuleCall moduleName function range context =
    let
        moduleUse : ModuleUse
        moduleUse =
            ModuleUse.new function range
    in
    context
        |> useBadAliasCall moduleName moduleUse
        |> useMissingAliasCall moduleName moduleUse


useBadAliasCall : ModuleName -> ModuleUse -> Module -> Module
useBadAliasCall moduleName moduleUse (Module context) =
    case moduleName of
        [ moduleAlias ] ->
            Module
                { context
                    | badAliases = BadAliasSet.use moduleAlias moduleUse context.badAliases
                }

        _ ->
            Module context


useMissingAliasCall : ModuleName -> ModuleUse -> Module -> Module
useMissingAliasCall moduleName moduleUse (Module context) =
    Module
        { context
            | missingAliases = MissingAliasSet.use moduleName moduleUse context.missingAliases
        }


foldBadAliases : (BadAlias -> a -> a) -> a -> Module -> a
foldBadAliases folder start (Module { badAliases }) =
    BadAliasSet.fold folder start badAliases


lookupModuleName : Module -> String -> Maybe ModuleName
lookupModuleName (Module { aliases }) moduleAlias =
    Dict.get moduleAlias aliases
        |> Maybe.map .from


foldMissingAliases : (MissingAlias -> a -> a) -> a -> Module -> a
foldMissingAliases folder start (Module { missingAliases }) =
    MissingAliasSet.fold folder start missingAliases


converter :
    Options
    ->
        { fromProjectToModule : Rule.ContextCreator Project Module
        , fromModuleToProject : Rule.ContextCreator Module Project
        , foldProjectContexts : Project -> Project -> Project
        }
converter options =
    { fromProjectToModule = Rule.initContextCreator (\_ -> initialModule)
    , fromModuleToProject =
        Rule.initContextCreator
            (\key moduleName filePath (Module { aliases }) ->
                if List.any (\excluded -> String.startsWith excluded filePath) options.doNotDiscoverIn then
                    initialProject

                else
                    Dict.singleton moduleName
                        { key = key
                        , aliases =
                            Dict.foldl
                                (\to { from, aliasRange } acc ->
                                    case options.lookupAlias from of
                                        Just _ ->
                                            -- The alias is already configured, no need to duplicate errors here
                                            acc

                                        Nothing ->
                                            Dict.insert from
                                                { aliasRange = aliasRange
                                                , to = to
                                                }
                                                acc
                                )
                                Dict.empty
                                aliases
                        }
                        |> Project
            )
            |> Rule.withModuleKey
            |> Rule.withModuleName
            |> Rule.withFilePath
    , foldProjectContexts =
        \(Project l) (Project r) ->
            Dict.union l r
                |> Project
    }


getAliasesGroups :
    Project
    ->
        List
            { from : ModuleName
            , tos :
                List
                    ( String
                    , List
                        { inModule : List String
                        , key : Rule.ModuleKey
                        , aliasRange : Range
                        }
                    )
            }
getAliasesGroups (Project project) =
    project
        |> Dict.foldl
            (\inModule { key, aliases } acc ->
                Dict.foldl
                    (\from { to, aliasRange } iacc ->
                        let
                            info :
                                { inModule : List String
                                , key : Rule.ModuleKey
                                , aliasRange : Range
                                }
                            info =
                                { inModule = inModule
                                , key = key
                                , aliasRange = aliasRange
                                }
                        in
                        case Dict.get from iacc of
                            Nothing ->
                                Dict.insert from (Dict.singleton to [ info ]) iacc

                            Just existing ->
                                Dict.insert from
                                    (case Dict.get to existing of
                                        Nothing ->
                                            Dict.insert to [ info ] existing

                                        Just e ->
                                            Dict.insert to (info :: e) existing
                                    )
                                    iacc
                    )
                    acc
                    aliases
            )
            Dict.empty
        |> Dict.foldl
            (\from tos acc -> { from = from, tos = Dict.toList tos } :: acc)
            []
