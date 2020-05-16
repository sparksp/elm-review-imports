module NoModuleOnExposedNames.Context exposing (Module, expose, initial, isExposedBy)

import Dict exposing (Dict)
import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.ModuleName exposing (ModuleName)


type Module
    = Expose (Dict ModuleName Exposing)


initial : Module
initial =
    Expose Dict.empty


expose : ModuleName -> Exposing -> Module -> Module
expose moduleName exposer (Expose exposes) =
    Expose (Dict.insert moduleName exposer exposes)


isExposedBy : Module -> ModuleName -> String -> Bool
isExposedBy (Expose exposes) moduleName name =
    case Dict.get moduleName exposes of
        Nothing ->
            False

        Just exposer ->
            Exposing.exposesFunction name exposer
