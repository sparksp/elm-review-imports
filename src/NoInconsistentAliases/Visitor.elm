module NoInconsistentAliases.Visitor exposing (rule)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import NoInconsistentAliases.BadAlias as BadAlias exposing (BadAlias)
import NoInconsistentAliases.Config as Config exposing (Config)
import NoInconsistentAliases.Context as Context
import NoInconsistentAliases.ModuleUse as ModuleUse exposing (ModuleUse)
import Review.Fix as Fix exposing (Fix)
import Review.Rule as Rule exposing (Error, Rule)


rule : Config -> Rule
rule config =
    Rule.newModuleRuleSchema "NoInconsistentAliases" Context.initial
        |> Rule.withImportVisitor (importVisitor config)
        |> Rule.withExpressionVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation (finalEvaluation config)
        |> Rule.fromModuleRuleSchema


importVisitor : Config -> Node Import -> Context.Module -> ( List (Error {}), Context.Module )
importVisitor config node context =
    let
        moduleName =
            node |> Node.value |> .moduleName |> Node.value

        maybeModuleAlias =
            node |> Node.value |> .moduleAlias |> Maybe.map (Node.map formatModuleName)
    in
    case ( Config.lookupAlias moduleName config, maybeModuleAlias ) of
        ( Just expectedAlias, Just moduleAlias ) ->
            if expectedAlias /= (moduleAlias |> Node.value) then
                let
                    badAlias =
                        BadAlias.new (Node.value moduleAlias) moduleName (Node.range moduleAlias)
                in
                ( [], Context.addBadAlias badAlias context )

            else
                ( [], context )

        _ ->
            ( [], context )


expressionVisitor : Node Expression -> Rule.Direction -> Context.Module -> ( List (Error {}), Context.Module )
expressionVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Expression.FunctionOrValue [ moduleAlias ] function ) ->
            ( [], Context.addModuleCall moduleAlias function (Node.range node) context )

        ( Rule.OnEnter, _ ) ->
            ( [], context )

        ( Rule.OnExit, _ ) ->
            ( [], context )


finalEvaluation : Config -> Context.Module -> List (Error {})
finalEvaluation config context =
    Context.mapBadAliases (incorrectAliasError config) context


incorrectAliasError : Config -> BadAlias -> Error {}
incorrectAliasError config badAlias =
    let
        expectedAlias =
            BadAlias.mapModuleName (\name -> Config.lookupAlias name config) badAlias
                |> Maybe.withDefault ""

        badRange =
            BadAlias.range badAlias

        fixModuleAlias =
            Fix.replaceRangeBy badRange expectedAlias

        fixModuleUses =
            BadAlias.mapUses (fixModuleUse expectedAlias) badAlias
    in
    Rule.errorWithFix
        { message = incorrectAliasMessage badAlias
        , details = [ "This import does not use your preferred alias " ++ quote expectedAlias ++ "." ]
        }
        badRange
        (fixModuleAlias :: fixModuleUses)


incorrectAliasMessage : BadAlias -> String
incorrectAliasMessage badAlias =
    "Incorrect alias "
        ++ BadAlias.mapName quote badAlias
        ++ " for module "
        ++ BadAlias.mapModuleName (formatModuleName >> quote) badAlias


fixModuleUse : String -> ModuleUse -> Fix
fixModuleUse expectedAlias use =
    Fix.replaceRangeBy (ModuleUse.range use) (ModuleUse.mapFunction (\name -> expectedAlias ++ "." ++ name) use)


formatModuleName : ModuleName -> String
formatModuleName moduleName =
    String.join "." moduleName


quote : String -> String
quote string =
    "`" ++ string ++ "`"
