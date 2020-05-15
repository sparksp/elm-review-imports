module NoInconsistentAliases.Context exposing
    ( Module, initial
    , addModuleAlias, lookupModuleName, ModuleNameLookup
    , addBadAlias, addModuleCall, foldBadAliases
    )

{-|

@docs Module, initial
@docs addModuleAlias, lookupModuleName, ModuleNameLookup
@docs addBadAlias, addModuleCall, foldBadAliases

-}

import Dict exposing (Dict)
import Elm.Syntax.Range exposing (Range)
import NoInconsistentAliases.BadAlias as BadAlias exposing (BadAlias)
import NoInconsistentAliases.BadAliasSet as BadAliasSet exposing (BadAliasSet)
import NoInconsistentAliases.ModuleUse as ModuleUse


type Module
    = Module
        { aliases : Dict String String
        , badAliases : BadAliasSet
        }


type alias ModuleNameLookup =
    String -> Maybe String


initial : Module
initial =
    Module
        { aliases = Dict.empty
        , badAliases = BadAliasSet.empty
        }


addModuleAlias : String -> String -> Module -> Module
addModuleAlias moduleName moduleAlias (Module context) =
    Module { context | aliases = Dict.insert moduleAlias moduleName context.aliases }


addBadAlias : BadAlias -> Module -> Module
addBadAlias badAlias (Module context) =
    Module { context | badAliases = BadAliasSet.insert badAlias context.badAliases }


addModuleCall : BadAlias.Name -> String -> Range -> Module -> Module
addModuleCall moduleAlias function range (Module context) =
    Module
        { context
            | badAliases = BadAliasSet.use moduleAlias (ModuleUse.new function range) context.badAliases
        }


foldBadAliases : (BadAlias -> a -> a) -> a -> Module -> a
foldBadAliases folder start (Module { badAliases }) =
    BadAliasSet.fold folder start badAliases


lookupModuleName : Module -> ModuleNameLookup
lookupModuleName (Module { aliases }) moduleAlias =
    Dict.get moduleAlias aliases
