module NoInconsistentAliases.Visitor.Options exposing (AliasLookup, Options, fromConfig)

import Elm.Syntax.ModuleName exposing (ModuleName)
import List.Nonempty exposing (Nonempty)
import NoInconsistentAliases.Config as Config exposing (Config)


type alias Options =
    { lookupAliases : AliasLookup
    , canMissAliases : Bool
    }


type alias AliasLookup =
    ModuleName -> Maybe (Nonempty String)


fromConfig : Config -> Options
fromConfig config =
    { lookupAliases = Config.lookupAliases config
    , canMissAliases = Config.canMissAliases config
    }
