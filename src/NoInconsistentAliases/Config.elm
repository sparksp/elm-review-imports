module NoInconsistentAliases.Config exposing (Config, config, lookupAlias)

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)


type Config
    = Aliases (Dict ModuleName String)


config : List ( ModuleName, String ) -> Config
config aliases =
    Aliases (Dict.fromList aliases)


lookupAlias : ModuleName -> Config -> Maybe String
lookupAlias moduleName (Aliases aliases) =
    Dict.get moduleName aliases
