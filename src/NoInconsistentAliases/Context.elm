module NoInconsistentAliases.Context exposing
    ( Project, Module, initial, withModuleContext
    , AliasName
    , addModuleAlias, lookupModuleName
    , addMissingAlias, foldMissingAliases
    , addBadAlias, foldBadAliases
    , addModuleCall
    , importedAliasesByModule, isPreferredAliasUsed
    )

{-|

@docs Project, Module, initial, withModuleContext
@docs AliasName, ModuleName
@docs addModuleAlias, lookupModuleName
@docs addMissingAlias, foldMissingAliases
@docs addBadAlias, foldBadAliases
@docs addModuleCall
@docs inconsistentModuleAliases

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
import NoInconsistentAliases.Visitor.Options as Options exposing (Options)
import Review.Rule as Rule


type alias AliasName =
    String


type Project
    = Project
        { importedModules : Dict ModuleName (Dict AliasName (List ( Rule.ModuleKey, Range )))
        , preferredAliasesUsed : Dict AliasName (List ( Rule.ModuleKey, ModuleName ))
        }


type Module
    = Module
        { aliases : Dict AliasName ( ModuleName, Range )
        , badAliases : BadAliasSet
        , missingAliases : MissingAliasSet
        }


initial : Project
initial =
    Project
        { importedModules = Dict.empty
        , preferredAliasesUsed = Dict.empty
        }


importedAliasesByModule : Project -> Dict ModuleName (Dict AliasName (List ( Rule.ModuleKey, Range )))
importedAliasesByModule (Project { importedModules }) =
    importedModules
        |> Dict.filter (\_ dict -> Dict.size dict > 1)


withModuleContext :
    Options
    ->
        Rule.ProjectRuleSchema
            { schemaState
                | canAddModuleVisitor : ()
                , withModuleContext : Rule.Required
            }
            Project
            Module
    ->
        Rule.ProjectRuleSchema
            { schemaState
                | hasAtLeastOneVisitor : ()
                , withModuleContext : Rule.Forbidden
            }
            Project
            Module
withModuleContext { lookupAlias } schema =
    schema
        |> Rule.withModuleContext
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject lookupAlias
            , foldProjectContexts = foldProjectContexts
            }


fromProjectToModule : Rule.ModuleKey -> Node ModuleName -> Project -> Module
fromProjectToModule _ _ _ =
    Module
        { aliases = Dict.empty
        , badAliases = BadAliasSet.empty
        , missingAliases = MissingAliasSet.empty
        }


fromModuleToProject : Options.AliasLookup -> Rule.ModuleKey -> Node ModuleName -> Module -> Project
fromModuleToProject lookupAlias moduleKey (Node _ moduleName) (Module { aliases }) =
    Project
        { importedModules = aliases |> fromModuleAliasesToProjectImportedModules moduleKey
        , preferredAliasesUsed = fromModuleAliasesToProjectPreferredAliasesUsed lookupAlias moduleKey aliases
        }


fromModuleAliasesToProjectImportedModules :
    Rule.ModuleKey
    -> Dict AliasName ( ModuleName, Range )
    -> Dict ModuleName (Dict AliasName (List ( Rule.ModuleKey, Range )))
fromModuleAliasesToProjectImportedModules moduleKey aliases =
    Dict.foldr (foldModuleAliasToProjectImportedModules moduleKey) Dict.empty aliases


fromModuleAliasesToProjectPreferredAliasesUsed :
    Options.AliasLookup
    -> Rule.ModuleKey
    -> Dict AliasName ( ModuleName, Range )
    -> Dict AliasName (List ( Rule.ModuleKey, ModuleName ))
fromModuleAliasesToProjectPreferredAliasesUsed lookupAlias moduleKey aliases =
    aliases
        |> Dict.filter (\key ( aliasName, _ ) -> lookupAlias aliasName == Just key)
        |> Dict.map (\aliasName ( moduleName, _ ) -> [ ( moduleKey, moduleName ) ])


foldModuleAliasToProjectImportedModules :
    Rule.ModuleKey
    -> AliasName
    -> ( ModuleName, Range )
    -> Dict ModuleName (Dict AliasName (List ( Rule.ModuleKey, Range )))
    -> Dict ModuleName (Dict AliasName (List ( Rule.ModuleKey, Range )))
foldModuleAliasToProjectImportedModules moduleKey aliasName ( moduleName, range ) importedModules =
    if moduleName == toModuleName aliasName then
        importedModules

    else
        Dict.insert moduleName (Dict.singleton aliasName [ ( moduleKey, range ) ]) importedModules


foldProjectContexts : Project -> Project -> Project
foldProjectContexts (Project a) (Project b) =
    Project
        { importedModules = Dict.merge Dict.insert mergeImportedModules Dict.insert a.importedModules b.importedModules Dict.empty
        , preferredAliasesUsed = Dict.foldr Dict.insert a.preferredAliasesUsed b.preferredAliasesUsed
        }


mergeImportedModules :
    ModuleName
    -> Dict AliasName (List ( Rule.ModuleKey, Range ))
    -> Dict AliasName (List ( Rule.ModuleKey, Range ))
    -> Dict ModuleName (Dict AliasName (List ( Rule.ModuleKey, Range )))
    -> Dict ModuleName (Dict AliasName (List ( Rule.ModuleKey, Range )))
mergeImportedModules moduleName a b dict =
    Dict.insert moduleName (foldModuleAliases a b) dict


foldModuleAliases :
    Dict AliasName (List ( Rule.ModuleKey, Range ))
    -> Dict AliasName (List ( Rule.ModuleKey, Range ))
    -> Dict AliasName (List ( Rule.ModuleKey, Range ))
foldModuleAliases a b =
    Dict.merge Dict.insert mergeModuleAliases Dict.insert a b Dict.empty


mergeModuleAliases :
    AliasName
    -> List ( Rule.ModuleKey, Range )
    -> List ( Rule.ModuleKey, Range )
    -> Dict AliasName (List ( Rule.ModuleKey, Range ))
    -> Dict AliasName (List ( Rule.ModuleKey, Range ))
mergeModuleAliases aliasName a b dict =
    Dict.insert aliasName (a ++ b) dict


addModuleAlias : ModuleName -> Node String -> Module -> Module
addModuleAlias moduleName (Node range moduleAlias) (Module context) =
    Module { context | aliases = Dict.insert moduleAlias ( moduleName, range ) context.aliases }


addBadAlias : BadAlias -> Module -> Module
addBadAlias badAlias (Module context) =
    Module { context | badAliases = BadAliasSet.insert badAlias context.badAliases }


addMissingAlias : MissingAlias -> Module -> Module
addMissingAlias missingAlias (Module context) =
    Module { context | missingAliases = MissingAliasSet.insert missingAlias context.missingAliases }


addModuleCall : ModuleName -> String -> Range -> Module -> Module
addModuleCall moduleName function range context =
    let
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
    BadAliasSet.foldr folder start badAliases


lookupModuleName : Module -> AliasName -> Maybe ModuleName
lookupModuleName (Module { aliases }) moduleAlias =
    Dict.get moduleAlias aliases
        |> Maybe.map Tuple.first


foldMissingAliases : (MissingAlias -> a -> a) -> a -> Module -> a
foldMissingAliases folder start (Module { missingAliases }) =
    MissingAliasSet.foldr folder start missingAliases


isPreferredAliasUsed : AliasName -> Project -> ( Rule.ModuleKey, Range ) -> Bool
isPreferredAliasUsed aliasName (Project { preferredAliasesUsed }) ( key, _ ) =
    Dict.get aliasName preferredAliasesUsed
        |> Maybe.withDefault []
        |> hasKey key


hasKey : Rule.ModuleKey -> List ( Rule.ModuleKey, ModuleName ) -> Bool
hasKey key locations =
    List.foldr (hasKeyHelper key) False locations


hasKeyHelper : Rule.ModuleKey -> ( Rule.ModuleKey, ModuleName ) -> Bool -> Bool
hasKeyHelper search ( value, key ) acc =
    if value == search then
        True

    else
        acc


toModuleName : String -> ModuleName
toModuleName =
    String.split "."
