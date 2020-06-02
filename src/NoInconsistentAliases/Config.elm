module NoInconsistentAliases.Config exposing
    ( Config, config, detectAliases, noMissingAliases
    , canMissAliases, lookupAlias
    )

{-|

@docs Config, config, detectAliases, noMissingAliases
@docs canMissAliases, lookupAlias

-}

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)


type Config
    = Config
        { aliases : Dict ModuleName String
        , allowMissingAliases : Bool
        , detectAliases : Bool
        }


config : List ( String, String ) -> Config
config aliases =
    Config
        { aliases =
            aliases
                |> List.map (Tuple.mapFirst toModuleName)
                |> Dict.fromList
        , allowMissingAliases = True
        , detectAliases = False
        }


detectAliases : Config -> Config
detectAliases (Config cfg) =
    Config { cfg | detectAliases = True }


noMissingAliases : Config -> Config
noMissingAliases (Config cfg) =
    Config { cfg | allowMissingAliases = False }


canMissAliases : Config -> Bool
canMissAliases (Config cfg) =
    cfg.allowMissingAliases


lookupAlias : Config -> ModuleName -> Maybe String
lookupAlias (Config { aliases }) moduleName =
    Dict.get moduleName aliases



--- HELPERS


toModuleName : String -> ModuleName
toModuleName moduleName =
    String.split "." moduleName
