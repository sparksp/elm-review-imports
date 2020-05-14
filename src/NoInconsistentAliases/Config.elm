module NoInconsistentAliases.Config exposing (Config, config, lookupAlias)

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)


type Config
    = Aliases (Dict ModuleName String)


config : List ( String, String ) -> Config
config aliases =
    aliases
        |> List.map (Tuple.mapFirst toModuleName)
        |> Dict.fromList
        |> Aliases


lookupAlias : String -> Config -> Maybe String
lookupAlias moduleName (Aliases aliases) =
    Dict.get (toModuleName moduleName) aliases


toModuleName : String -> ModuleName
toModuleName moduleName =
    String.split "." moduleName
