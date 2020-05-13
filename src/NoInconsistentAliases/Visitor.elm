module NoInconsistentAliases.Visitor exposing (rule)

import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import NoInconsistentAliases.Config as Config exposing (Config)
import Review.Rule as Rule exposing (Error, Rule)


rule : Config -> Rule
rule config =
    Rule.newModuleRuleSchema "NoInconsistentAliases" ()
        |> Rule.withSimpleImportVisitor (importVisitor config)
        |> Rule.fromModuleRuleSchema


importVisitor : Config -> Node Import -> List (Error {})
importVisitor config node =
    let
        moduleName =
            node |> Node.value |> .moduleName |> Node.value

        maybeModuleAlias =
            node |> Node.value |> .moduleAlias |> Maybe.map (Node.map formatModuleName)
    in
    case ( Config.lookupAlias moduleName config, maybeModuleAlias ) of
        ( Just expectedAlias, Just moduleAlias ) ->
            if expectedAlias /= (moduleAlias |> Node.value) then
                [ incorrectAliasError expectedAlias moduleName moduleAlias ]

            else
                []

        _ ->
            []


incorrectAliasError : String -> ModuleName -> Node String -> Error {}
incorrectAliasError expectedAlias moduleName wrongAlias =
    Rule.error
        { message = "Incorrect alias `" ++ Node.value wrongAlias ++ "` for module `" ++ formatModuleName moduleName ++ "`"
        , details = [ "This import does not use your preferred alias `" ++ expectedAlias ++ "`." ]
        }
        (Node.range wrongAlias)


formatModuleName : ModuleName -> String
formatModuleName moduleName =
    String.join "." moduleName
