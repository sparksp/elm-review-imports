module NoInconsistentAliases.Visitor exposing (rule)

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type as Type
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
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
        |> Rule.withDeclarationListVisitor declarationListVisitor
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


declarationListVisitor : List (Node Declaration) -> Context.Module -> ( List (Error {}), Context.Module )
declarationListVisitor nodes context =
    ( [], List.foldl declarationVisitor context nodes )


declarationVisitor : Node Declaration -> Context.Module -> Context.Module
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { signature } ->
            context |> maybeSignatureVisitor signature

        Declaration.AliasDeclaration { typeAnnotation } ->
            context |> typeAnnotationVisitor typeAnnotation

        Declaration.CustomTypeDeclaration { constructors } ->
            context |> valueConstructorListVisitor constructors

        _ ->
            context


maybeSignatureVisitor : Maybe (Node Signature) -> Context.Module -> Context.Module
maybeSignatureVisitor maybeNode context =
    case maybeNode of
        Just node ->
            context |> signatureVisitor node

        Nothing ->
            context


signatureVisitor : Node Signature -> Context.Module -> Context.Module
signatureVisitor node context =
    typeAnnotationVisitor (node |> Node.value |> .typeAnnotation) context


valueConstructorListVisitor : List (Node Type.ValueConstructor) -> Context.Module -> Context.Module
valueConstructorListVisitor list context =
    List.foldl valueConstructorVisitor context list


valueConstructorVisitor : Node Type.ValueConstructor -> Context.Module -> Context.Module
valueConstructorVisitor node context =
    context |> typeAnnotationListVisitor (node |> Node.value |> .arguments)


typeAnnotationListVisitor : List (Node TypeAnnotation) -> Context.Module -> Context.Module
typeAnnotationListVisitor list context =
    List.foldl typeAnnotationVisitor context list


typeAnnotationVisitor : Node TypeAnnotation -> Context.Module -> Context.Module
typeAnnotationVisitor node context =
    case Node.value node of
        TypeAnnotation.Typed moduleCall types ->
            context
                |> moduleCallVisitor moduleCall
                |> typeAnnotationListVisitor types

        TypeAnnotation.FunctionTypeAnnotation argument return ->
            context
                |> typeAnnotationVisitor argument
                |> typeAnnotationVisitor return

        _ ->
            context


moduleCallVisitor : Node ( ModuleName, String ) -> Context.Module -> Context.Module
moduleCallVisitor node context =
    case Node.value node of
        ( [ moduleAlias ], function ) ->
            Context.addModuleCall moduleAlias function (Node.range node) context

        _ ->
            context


expressionVisitor : Node Expression -> Rule.Direction -> Context.Module -> ( List (Error {}), Context.Module )
expressionVisitor node direction context =
    case ( direction, Node.value node ) of
        ( Rule.OnEnter, Expression.FunctionOrValue [ moduleAlias ] function ) ->
            ( [], Context.addModuleCall moduleAlias function (Node.range node) context )

        ( Rule.OnEnter, Expression.CaseExpression { cases } ) ->
            ( [], context |> caseListVisitor cases )

        ( Rule.OnEnter, _ ) ->
            ( [], context )

        ( Rule.OnExit, _ ) ->
            ( [], context )


caseListVisitor : List Expression.Case -> Context.Module -> Context.Module
caseListVisitor list context =
    List.foldl caseVisitor context list


caseVisitor : Expression.Case -> Context.Module -> Context.Module
caseVisitor ( pattern, _ ) context =
    context |> patternVisitor pattern


patternVisitor : Node Pattern -> Context.Module -> Context.Module
patternVisitor node context =
    case Node.value node of
        Pattern.NamedPattern { moduleName, name } _ ->
            case moduleName of
                [ moduleAlias ] ->
                    let
                        { start } =
                            Node.range node

                        newEnd =
                            { start | column = start.column + ([ moduleAlias, name ] |> formatModuleName |> String.length) }

                        range =
                            { start = start, end = newEnd }
                    in
                    context |> Context.addModuleCall moduleAlias name range

                _ ->
                    context

        _ ->
            context


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
