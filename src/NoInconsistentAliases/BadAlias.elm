module NoInconsistentAliases.BadAlias exposing (BadAlias, mapExpectedNames, mapModuleName, mapName, mapUses, new, range, withModuleUse)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Range exposing (Range)
import List.Nonempty exposing (Nonempty)
import NoInconsistentAliases.ModuleUse exposing (ModuleUse)


type BadAlias
    = BadAlias
        { name : String
        , moduleName : ModuleName
        , expectedNames : Nonempty String
        , at : Range
        , uses : List ModuleUse
        }


new : { name : String, moduleName : ModuleName, expectedNames : Nonempty String, range : Range } -> BadAlias
new options =
    BadAlias
        { name = options.name
        , moduleName = options.moduleName
        , expectedNames = options.expectedNames
        , at = options.range
        , uses = []
        }


withModuleUse : ModuleUse -> BadAlias -> BadAlias
withModuleUse moduleUse (BadAlias alias) =
    BadAlias { alias | uses = moduleUse :: alias.uses }


range : BadAlias -> Range
range (BadAlias alias) =
    alias.at


mapName : (String -> a) -> BadAlias -> a
mapName mapper (BadAlias alias) =
    mapper alias.name


mapModuleName : (ModuleName -> a) -> BadAlias -> a
mapModuleName mapper (BadAlias alias) =
    mapper alias.moduleName


mapExpectedNames : (Nonempty String -> a) -> BadAlias -> a
mapExpectedNames mapper (BadAlias alias) =
    mapper alias.expectedNames


mapUses : (ModuleUse -> a) -> BadAlias -> List a
mapUses mapper (BadAlias { uses }) =
    List.map mapper uses
