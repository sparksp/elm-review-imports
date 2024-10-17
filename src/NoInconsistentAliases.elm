module NoInconsistentAliases exposing
    ( rule, config, noMissingAliases
    , Config
    , discoverAliasesAndShowIndividual, discoverAliasesAndShowSummary, doNotDiscoverIn
    )

{-|

@docs rule, config, noMissingAliases
@docs Config
@docs discoverAliasesAndShowIndividual, discoverAliasesAndShowSummary, doNotDiscoverIn

-}

import NoInconsistentAliases.Config as Config
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


{-| Automatically discover which aliases to use based on usage in your project.

Shows a summary of mismatches - you should use this when first adding NoInconsistentAliases to your project.

-}
discoverAliasesAndShowSummary : Config -> Config
discoverAliasesAndShowSummary =
    Config.discoverAliases Config.DiscoverAndShowSummary


{-| Automatically discover which aliases to use based on usage in your project.

Shows individual errors for mismatches - you should use this after the first pass of fixes.

-}
discoverAliasesAndShowIndividual : Config -> Config
discoverAliasesAndShowIndividual =
    Config.discoverAliases Config.DiscoverAndShowIndividual


{-| Configuration for the NoInconsistentAliases rule.
-}
type alias Config =
    Config.Config


{-| Ignore specific folders when doing discovery.
-}
doNotDiscoverIn : List String -> Config -> Config
doNotDiscoverIn ignored cfg =
    Config.doNotDiscoverIn ignored cfg
