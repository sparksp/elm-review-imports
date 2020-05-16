module NoInconsistentAliases exposing (rule, config, noMissingAliases)

{-|

@docs rule, config, noMissingAliases

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
        |> NoInconsistentAliases.rule

-}
config : List ( String, String ) -> Config
config =
    Config.config


{-| If a module is called, ensure that there are no missing aliases.

    NoInconsistentAliases.config
        [ ( "Html.Attributes", "Attr" )
        ]
        |> NoInconsistentAliases.noMissingAliases
        |> NoInconsistentAliases.rule


## Success

    import Html.Attributes exposing (class)

    view =
        div [ class "flex" ] []


## Failure

    import Html.Attributes

    container children =
        Html.div [ Html.Attributes.class "container" ] children

-}
noMissingAliases : Config -> Config
noMissingAliases =
    Config.noMissingAliases
