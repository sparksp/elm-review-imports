module NoInconsistentAliases.MissingAlias exposing (MissingAlias, hasUses, mapExpectedNames, mapModuleName, mapUses, new, range, withModuleUse)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import List.Nonempty exposing (Nonempty)
import NoInconsistentAliases.ModuleUse exposing (ModuleUse)


type MissingAlias
    = MissingAlias
        { moduleName : ModuleName
        , expectedNames : Nonempty String
        , at : Range
        , uses : List ModuleUse
        }


new : ModuleName -> Nonempty String -> Range -> MissingAlias
new newModuleName newExpectedNames newRange =
    MissingAlias
        { moduleName = newModuleName
        , expectedNames = newExpectedNames
        , at = newRange
        , uses = []
        }


withModuleUse : ModuleUse -> MissingAlias -> MissingAlias
withModuleUse moduleUse (MissingAlias alias) =
    MissingAlias { alias | uses = moduleUse :: alias.uses }


hasUses : MissingAlias -> Bool
hasUses (MissingAlias { uses }) =
    uses /= []


mapModuleName : (ModuleName -> a) -> MissingAlias -> a
mapModuleName mapper (MissingAlias { moduleName }) =
    mapper moduleName


mapExpectedNames : (Nonempty String -> a) -> MissingAlias -> a
mapExpectedNames mapper (MissingAlias { expectedNames }) =
    mapper expectedNames


mapUses : (ModuleUse -> a) -> MissingAlias -> List a
mapUses mapper (MissingAlias { uses }) =
    List.map mapper uses


range : MissingAlias -> Range
range (MissingAlias { at }) =
    at
