module NoInconsistentAliases.Config exposing
    ( Config, config, noMissingAliases
    , canMissAliases, listAliases
    )

{-|

@docs Config, config, noMissingAliases
@docs canMissAliases, lookupAliases, listAliases

-}

import Elm.Syntax.ModuleName exposing (ModuleName)
import List.Nonempty as Nonempty exposing (Nonempty)


type Config
    = Config
        { aliases : List ( ModuleName, Nonempty String )
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
        , allowMissingAliases = True
        }


noMissingAliases : Config -> Config
noMissingAliases (Config cfg) =
    Config { cfg | allowMissingAliases = False }


canMissAliases : Config -> Bool
canMissAliases (Config cfg) =
    cfg.allowMissingAliases


listAliases : Config -> List ( ModuleName, Nonempty String )
listAliases (Config { aliases }) =
    aliases



--- HELPERS


toModuleName : String -> ModuleName
toModuleName =
    String.split "."
