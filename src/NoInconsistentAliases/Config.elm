module NoInconsistentAliases.Config exposing (AliasLookup, Config, config, lookupAlias)

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)


type Config
    = Aliases (Dict ModuleName String)


type alias AliasLookup =
    String -> Maybe String


config : List ( String, String ) -> Config
config aliases =
    aliases
        |> List.map (Tuple.mapFirst toModuleName)
        |> Dict.fromList
        |> Aliases


lookupAlias : Config -> AliasLookup
lookupAlias (Aliases aliases) moduleName =
    Dict.get (toModuleName moduleName) aliases


toModuleName : String -> ModuleName
toModuleName moduleName =
    String.split "." moduleName
