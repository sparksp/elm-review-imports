module NoInconsistentAliases.Config exposing
    ( Config, config, noMissingAliases
    , discoverAliases, DiscoverAliases(..), doNotDiscoverIn
    , canMissAliases, lookupAlias, shouldDiscoverAliases, shouldNotDiscoverIn
    )

{-|

@docs Config, config, noMissingAliases
@docs discoverAliases, DiscoverAliases, doNotDiscoverIn
@docs canMissAliases, lookupAlias, shouldDiscoverAliases, shouldNotDiscoverIn

-}

import Dict exposing (Dict)
import Elm.Syntax.ModuleName exposing (ModuleName)


type Config
    = Config
        { aliases : Dict ModuleName String
        , allowMissingAliases : Bool
        , discoverAliases : DiscoverAliases
        , doNotDiscoverIn : List String
        }


type DiscoverAliases
    = DoNotDiscover
    | DiscoverAndShowSummary
    | DiscoverAndShowIndividual


config : List ( String, String ) -> Config
config aliases =
    Config
        { aliases =
            aliases
                |> List.map (Tuple.mapFirst toModuleName)
                |> Dict.fromList
        , allowMissingAliases = True
        , discoverAliases = DoNotDiscover
        , doNotDiscoverIn = []
        }


noMissingAliases : Config -> Config
noMissingAliases (Config cfg) =
    Config { cfg | allowMissingAliases = False }


discoverAliases : DiscoverAliases -> Config -> Config
discoverAliases discover (Config cfg) =
    Config { cfg | discoverAliases = discover }


doNotDiscoverIn : List String -> Config -> Config
doNotDiscoverIn ignored (Config cfg) =
    Config { cfg | doNotDiscoverIn = ignored }


canMissAliases : Config -> Bool
canMissAliases (Config cfg) =
    cfg.allowMissingAliases


lookupAlias : Config -> ModuleName -> Maybe String
lookupAlias (Config { aliases }) moduleName =
    Dict.get moduleName aliases


shouldDiscoverAliases : Config -> DiscoverAliases
shouldDiscoverAliases (Config cfg) =
    cfg.discoverAliases


shouldNotDiscoverIn : Config -> List String
shouldNotDiscoverIn (Config cfg) =
    cfg.doNotDiscoverIn



--- HELPERS


toModuleName : String -> ModuleName
toModuleName moduleName =
    String.split "." moduleName
