module NoModuleOnExposedNames exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Range exposing (Range)
import NoModuleOnExposedNames.Context as Context
import Review.Fix as Fix
import Review.Rule as Rule exposing (Error, Rule)
import Vendor.NameVisitor as NameVisitor


{-| Forbid modules on names that have been exposed.

    config : List Rule
    config =
        [ NoModuleOnExposedNames.rule
        ]


## Failure

    import Html.Attributes as Attr exposing (class)

    view children =
        div [ Attr.class "container" ] children

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoModuleOnExposedNames" Context.initial
        |> Rule.withImportVisitor importVisitor
        |> NameVisitor.withNameVisitor nameVisitor
        |> Rule.fromModuleRuleSchema


importVisitor : Node Import -> Context.Module -> ( List (Error {}), Context.Module )
importVisitor node context =
    ( [], context |> rememberExposedNames (Node.value node) )


rememberExposedNames : Import -> Context.Module -> Context.Module
rememberExposedNames { moduleName, moduleAlias, exposingList } context =
    case exposingList of
        Nothing ->
            context

        Just exposes ->
            let
                moduleNameOrAlias =
                    moduleAlias
                        |> Maybe.map Node.value
                        |> Maybe.withDefault (Node.value moduleName)
            in
            context |> Context.expose moduleNameOrAlias (Node.value exposes)


nameVisitor : Node ( ModuleName, String ) -> Context.Module -> ( List (Error {}), Context.Module )
nameVisitor node context =
    case Node.value node of
        ( moduleName, name ) ->
            if Context.isExposedBy context moduleName name then
                ( [ moduleOnExposedNameError name (Node.range node) ]
                , context
                )

            else
                ( [], context )


moduleOnExposedNameError : String -> Range -> Error {}
moduleOnExposedNameError name range =
    Rule.errorWithFix
        { message = "Module used on exposed name `" ++ name ++ "`."
        , details =
            [ "It is not necessary to use the module here as `" ++ name ++ "` was exposed on import."
            , "You should remove the module from this call, or remove the name from the import .. exposing list."
            ]
        }
        range
        [ Fix.replaceRangeBy range name ]
