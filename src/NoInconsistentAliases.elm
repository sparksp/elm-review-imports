module NoInconsistentAliases exposing (rule, config)

{-|

@docs rule, config

-}

import NoInconsistentAliases.Config as Config exposing (Config)
import NoInconsistentAliases.Visitor as Visitor
import Review.Rule exposing (Rule)


{-| Ensure consistent use of import aliases throughout your project.

    config : List Rule
    config =
        [ NoInconsistentAliases.config
            [ ( "Html.Attributes", "Attr" )
            ]
            |> NoInconsistentAliases.rule
        ]

-}
rule : Config -> Rule
rule =
    Visitor.rule


{-| Provide a list of preferred names to be enforced. If we find any of the given modules imported with a different alias we will report them.

    NoInconsistentAliases.config
        [ ( "Html.Attributes", "Attr" )
        , ( "Json.Decode", "Decode" )
        , ( "Json.Encode", "Encode" )
        ]

-}
config : List ( String, String ) -> Config
config =
    Config.config
