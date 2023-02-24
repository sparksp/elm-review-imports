module NoInconsistentAliases.Visitor.Options exposing (AliasLookup, Options, fromConfig)

import Elm.Syntax.ModuleName exposing (ModuleName)
import NoInconsistentAliases.Config as Config exposing (Config)
import RecordWithoutConstructorFunction exposing (RecordWithoutConstructorFunction)


type alias Options =
    RecordWithoutConstructorFunction
        { lookupAlias : AliasLookup
        , canMissAliases : Bool
        }


type alias AliasLookup =
    ModuleName -> Maybe String


fromConfig : Config -> Options
fromConfig config =
    { lookupAlias = Config.lookupAlias config
    , canMissAliases = Config.canMissAliases config
    }
