module NoInconsistentAliases.Config exposing (Config, config, lookupAlias)

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)


type Config
    = Config { aliases : Dict ModuleName String }


config : List ( ModuleName, String ) -> Config
config aliases =
    Config { aliases = Dict.fromList aliases }


lookupAlias : ModuleName -> Config -> Maybe String
lookupAlias moduleName (Config { aliases }) =
    Dict.get moduleName aliases
