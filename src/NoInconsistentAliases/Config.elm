module NoInconsistentAliases.Config exposing
    ( Config, config, noMissingAliases
    , canMissAliases, lookupAliases
    )

{-|

@docs Config, config, noMissingAliases
@docs canMissAliases, lookupAliases

-}

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)
import List.Nonempty as Nonempty exposing (Nonempty)


type Config
    = Config
        { aliases : Dict ModuleName (Nonempty String)
        , allowMissingAliases : Bool
        }


config : List ( String, String, List String ) -> Config
config aliases =
    Config
        { aliases =
            aliases
                |> List.map
                    (\( moduleName, primaryAlias, otherAliases ) ->
                        ( toModuleName moduleName
                        , Nonempty.fromElement primaryAlias
                            |> Nonempty.replaceTail otherAliases
                        )
                    )
                |> Dict.fromList
        , allowMissingAliases = True
        }


noMissingAliases : Config -> Config
noMissingAliases (Config cfg) =
    Config { cfg | allowMissingAliases = False }


canMissAliases : Config -> Bool
canMissAliases (Config cfg) =
    cfg.allowMissingAliases


lookupAliases : Config -> ModuleName -> Maybe (Nonempty String)
lookupAliases (Config { aliases }) moduleName =
    Dict.get moduleName aliases



--- HELPERS


toModuleName : String -> ModuleName
toModuleName moduleName =
    String.split "." moduleName
