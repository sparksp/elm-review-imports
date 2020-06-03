module NoInconsistentAliases exposing (rule, config, detectAliases, noMissingAliases)

{-|

@docs rule, config, detectAliases, noMissingAliases

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
            |> NoInconsistentAliases.detectAliases
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


{-| Compare the import aliases across your project and report any that are inconsistent.

    NoInconsistentAliases.config []
        |> NoInconsistentAliases.detectAliases
        |> NoInconsistentAliases.rule

If there's one alias that's used more than any other then the reports will recommend using that alias, otherwise the reports will list the aliases used for importing the module. This option does not offer fixes because you can use the [`config`](#config) list to do just that.

-}
detectAliases : Config -> Config
detectAliases =
    Config.detectAliases


{-| Ensure that imports are aliased if a module is used to qualify a function or type, and has a known alias.

    NoInconsistentAliases.config
        [ ( "Html.Attributes", "Attr" )
        ]
        |> NoInconsistentAliases.noMissingAliases
        |> NoInconsistentAliases.rule


## Failure

Here `Html.Attributes` has been used to call `class` and the preferred alias has not been used.

    import Html.Attributes

    view children =
        div [ Html.Attributes.class "container" ] children


## Success

Here `Html.Attributes` has been aliased to `Attr` as expected.

    import Html.Attributes as Attr

    view children =
        div [ Attr.class "container" ] children


## Success

Here `class` has been exposed so the alias is not needed.

    import Html.Attributes exposing (class)

    view children =
        div [ class "container" ] children

-}
noMissingAliases : Config -> Config
noMissingAliases =
    Config.noMissingAliases
