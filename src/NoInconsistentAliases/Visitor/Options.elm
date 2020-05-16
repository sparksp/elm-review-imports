module NoInconsistentAliases.Visitor.Options exposing (AliasLookup, Options, fromConfig)

import NoInconsistentAliases.Config as Config exposing (Config)


type alias Options =
    { lookupAlias : AliasLookup
    , canMissAliases : Bool
    }


type alias AliasLookup =
    String -> Maybe String


fromConfig : Config -> Options
fromConfig config =
    { lookupAlias = Config.lookupAlias config
    , canMissAliases = Config.canMissAliases config
    }
