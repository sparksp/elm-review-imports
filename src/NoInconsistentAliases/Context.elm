module NoInconsistentAliases.Context exposing (Module, addBadAlias, addModuleCall, initial, mapBadAliases)

import Elm.Syntax.Range exposing (Range)
import NoInconsistentAliases.BadAlias as BadAlias exposing (BadAlias)
import NoInconsistentAliases.BadAliasSet as BadAliasSet exposing (BadAliasSet)
import NoInconsistentAliases.ModuleUse as ModuleUse


type Module
    = BadAliases BadAliasSet


initial : Module
initial =
    BadAliases BadAliasSet.empty


addBadAlias : BadAlias -> Module -> Module
addBadAlias badAlias (BadAliases aliases) =
    BadAliases (BadAliasSet.insert badAlias aliases)


addModuleCall : BadAlias.Name -> String -> Range -> Module -> Module
addModuleCall moduleAlias function range (BadAliases aliases) =
    BadAliases (BadAliasSet.use moduleAlias (ModuleUse.new function range) aliases)


mapBadAliases : (BadAlias -> a) -> Module -> List a
mapBadAliases mapper (BadAliases aliases) =
    BadAliasSet.map mapper aliases
