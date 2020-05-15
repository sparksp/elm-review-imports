module NoInconsistentAliases.BadAlias exposing (BadAlias, Name, mapExpectedName, mapModuleName, mapName, mapUses, new, range, withModuleUse)

import Elm.Syntax.Range exposing (Range)
import NoInconsistentAliases.ModuleUse exposing (ModuleUse)


type BadAlias
    = BadAlias
        { name : Name
        , moduleName : String
        , expectedName : String
        , at : Range
        , uses : List ModuleUse
        }


type alias Name =
    String


new : Name -> String -> String -> Range -> BadAlias
new newName newModuleName newExpectedName newRange =
    BadAlias
        { name = newName
        , moduleName = newModuleName
        , expectedName = newExpectedName
        , at = newRange
        , uses = []
        }


withModuleUse : ModuleUse -> BadAlias -> BadAlias
withModuleUse moduleUse (BadAlias alias) =
    BadAlias { alias | uses = moduleUse :: alias.uses }


range : BadAlias -> Range
range (BadAlias alias) =
    alias.at


mapName : (Name -> a) -> BadAlias -> a
mapName mapper (BadAlias alias) =
    mapper alias.name


mapModuleName : (String -> a) -> BadAlias -> a
mapModuleName mapper (BadAlias alias) =
    mapper alias.moduleName


mapExpectedName : (String -> a) -> BadAlias -> a
mapExpectedName mapper (BadAlias alias) =
    mapper alias.expectedName


mapUses : (ModuleUse -> a) -> BadAlias -> List a
mapUses mapper (BadAlias { uses }) =
    List.map mapper uses
