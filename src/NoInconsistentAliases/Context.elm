module NoInconsistentAliases.Context exposing
    ( Module, initial
    , addModuleAlias, getModuleForAlias
    , addBadAlias, addModuleCall, foldBadAliases
    )

{-|

@docs Module, initial
@docs addModuleAlias, getModuleForAlias
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


getModuleForAlias : String -> Module -> Maybe String
getModuleForAlias moduleAlias (Module { aliases }) =
    Dict.get moduleAlias aliases
