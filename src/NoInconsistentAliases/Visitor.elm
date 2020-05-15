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
    let
        lookupAlias : Config.AliasLookup
        lookupAlias =
            Config.lookupAlias config
    in
    Rule.newModuleRuleSchema "NoInconsistentAliases" Context.initial
        |> Rule.withImportVisitor (importVisitor lookupAlias)
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withExpressionVisitor expressionVisitor
        |> Rule.withFinalModuleEvaluation (finalEvaluation lookupAlias)
        |> Rule.fromModuleRuleSchema


importVisitor : Config.AliasLookup -> Node Import -> Context.Module -> ( List (Error {}), Context.Module )
importVisitor lookupAlias node context =
    let
        moduleName =
            node |> Node.value |> .moduleName |> Node.value |> formatModuleName

        maybeModuleAlias =
            node |> Node.value |> .moduleAlias |> Maybe.map (Node.map formatModuleName)
    in
    ( []
    , context
        |> rememberModuleName moduleName
        |> rememberModuleAlias moduleName maybeModuleAlias
        |> rememberBadAlias lookupAlias moduleName maybeModuleAlias
    )


rememberModuleName : String -> Context.Module -> Context.Module
rememberModuleName moduleName context =
    context |> Context.addModuleAlias moduleName moduleName


rememberModuleAlias : String -> Maybe (Node String) -> Context.Module -> Context.Module
rememberModuleAlias moduleName maybeModuleAlias context =
    case maybeModuleAlias of
        Just moduleAlias ->
            context |> Context.addModuleAlias moduleName (Node.value moduleAlias)

        Nothing ->
            context


rememberBadAlias : Config.AliasLookup -> String -> Maybe (Node String) -> Context.Module -> Context.Module
rememberBadAlias lookupAlias moduleName maybeModuleAlias context =
    case ( lookupAlias moduleName, maybeModuleAlias ) of
        ( Just expectedAlias, Just moduleAlias ) ->
            if expectedAlias /= (moduleAlias |> Node.value) then
                let
                    badAlias =
                        BadAlias.new (Node.value moduleAlias) moduleName expectedAlias (Node.range moduleAlias)
                in
                context |> Context.addBadAlias badAlias

            else
                context

        _ ->
            context


declarationListVisitor : List (Node Declaration) -> Context.Module -> ( List (Error {}), Context.Module )
declarationListVisitor nodes context =
    ( [], List.foldl declarationVisitor context nodes )


declarationVisitor : Node Declaration -> Context.Module -> Context.Module
declarationVisitor node context =
    case Node.value node of
        Declaration.FunctionDeclaration { signature, declaration } ->
            context
                |> maybeSignatureVisitor signature
                |> functionImplementationVisitor declaration

        Declaration.AliasDeclaration { typeAnnotation } ->
            context |> typeAnnotationVisitor typeAnnotation

        Declaration.CustomTypeDeclaration { constructors } ->
            context |> valueConstructorListVisitor constructors

        _ ->
            context


functionImplementationVisitor : Node Expression.FunctionImplementation -> Context.Module -> Context.Module
functionImplementationVisitor node context =
    context |> patternListVisitor (node |> Node.value |> .arguments)


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

        TypeAnnotation.Tupled list ->
            context
                |> typeAnnotationListVisitor list

        TypeAnnotation.Record list ->
            context
                |> recordFieldListVisitor list

        TypeAnnotation.GenericRecord _ list ->
            context
                |> recordFieldListVisitor (Node.value list)

        TypeAnnotation.FunctionTypeAnnotation argument return ->
            context
                |> typeAnnotationVisitor argument
                |> typeAnnotationVisitor return

        _ ->
            context


recordFieldListVisitor : List (Node TypeAnnotation.RecordField) -> Context.Module -> Context.Module
recordFieldListVisitor list context =
    List.foldl recordFieldVisitor context list


recordFieldVisitor : Node TypeAnnotation.RecordField -> Context.Module -> Context.Module
recordFieldVisitor node context =
    context
        |> typeAnnotationVisitor (node |> Node.value |> Tuple.second)


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

        ( Rule.OnEnter, Expression.LetExpression { declarations } ) ->
            ( [], context |> letDeclarationListVisitor declarations )

        ( Rule.OnEnter, Expression.LambdaExpression { args } ) ->
            ( [], context |> patternListVisitor args )

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


letDeclarationListVisitor : List (Node Expression.LetDeclaration) -> Context.Module -> Context.Module
letDeclarationListVisitor list context =
    List.foldl letDeclarationVisitor context list


letDeclarationVisitor : Node Expression.LetDeclaration -> Context.Module -> Context.Module
letDeclarationVisitor node context =
    case Node.value node of
        Expression.LetFunction { signature, declaration } ->
            context
                |> maybeSignatureVisitor signature
                |> functionImplementationVisitor declaration

        Expression.LetDestructuring pattern _ ->
            context |> patternVisitor pattern


patternListVisitor : List (Node Pattern) -> Context.Module -> Context.Module
patternListVisitor list context =
    List.foldl patternVisitor context list


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

        Pattern.AsPattern pattern _ ->
            context |> patternVisitor pattern

        Pattern.ParenthesizedPattern pattern ->
            context |> patternVisitor pattern

        Pattern.ListPattern patterns ->
            context |> patternListVisitor patterns

        Pattern.TuplePattern patterns ->
            context |> patternListVisitor patterns

        Pattern.UnConsPattern head rest ->
            context
                |> patternVisitor head
                |> patternVisitor rest

        _ ->
            context


finalEvaluation : Config.AliasLookup -> Context.Module -> List (Error {})
finalEvaluation lookupAlias context =
    context |> Context.foldBadAliases (foldBadAliasError lookupAlias context) []


foldBadAliasError : Config.AliasLookup -> Context.Module -> BadAlias -> List (Error {}) -> List (Error {})
foldBadAliasError lookupAlias context badAlias errors =
    let
        moduleName =
            badAlias |> BadAlias.mapModuleName identity

        expectedAlias =
            badAlias |> BadAlias.mapExpectedName identity

        moduleClash =
            detectModuleCollision context moduleName expectedAlias

        aliasClash =
            moduleClash |> Maybe.andThen lookupAlias
    in
    case ( aliasClash, moduleClash ) of
        ( Just _, _ ) ->
            errors

        ( Nothing, Just collisionName ) ->
            Rule.error (collisionAliasMessage collisionName expectedAlias badAlias) (BadAlias.range badAlias)
                :: errors

        ( Nothing, Nothing ) ->
            let
                badRange =
                    BadAlias.range badAlias

                fixes =
                    Fix.replaceRangeBy badRange expectedAlias
                        :: BadAlias.mapUses (fixModuleUse expectedAlias) badAlias
            in
            Rule.errorWithFix (incorrectAliasMessage expectedAlias badAlias) badRange fixes
                :: errors


detectModuleCollision : Context.Module -> String -> String -> Maybe String
detectModuleCollision context moduleName expectedAlias =
    context
        |> Context.getModuleForAlias expectedAlias
        |> Maybe.andThen
            (\collisionName ->
                if collisionName == moduleName then
                    Nothing

                else
                    Just collisionName
            )


incorrectAliasMessage : String -> BadAlias -> { message : String, details : List String }
incorrectAliasMessage expectedAlias badAlias =
    let
        moduleName =
            BadAlias.mapModuleName quote badAlias
    in
    { message =
        "Incorrect alias " ++ BadAlias.mapName quote badAlias ++ " for module " ++ moduleName ++ "."
    , details =
        [ "This import does not use your preferred alias " ++ quote expectedAlias ++ " for " ++ moduleName ++ "."
        , "You should update the alias to be consistent with the rest of the project. "
            ++ "Remember to change all references to the alias in this module too."
        ]
    }


collisionAliasMessage : String -> String -> BadAlias -> { message : String, details : List String }
collisionAliasMessage collisionName expectedAlias badAlias =
    let
        moduleName =
            BadAlias.mapModuleName quote badAlias
    in
    { message =
        "Incorrect alias " ++ BadAlias.mapName quote badAlias ++ " for module " ++ moduleName ++ "."
    , details =
        [ "This import does not use your preferred alias " ++ quote expectedAlias ++ " for " ++ moduleName ++ "."
        , "Your preferred alias has already been taken by " ++ quote collisionName ++ "."
        , "You should change the alias for both modules to be consistent with the rest of the project. "
            ++ "Remember to change all references to the alias in this module too."
        ]
    }


fixModuleUse : String -> ModuleUse -> Fix
fixModuleUse expectedAlias use =
    Fix.replaceRangeBy (ModuleUse.range use) (ModuleUse.mapFunction (\name -> expectedAlias ++ "." ++ name) use)


formatModuleName : ModuleName -> String
formatModuleName moduleName =
    String.join "." moduleName


quote : String -> String
quote string =
    "`" ++ string ++ "`"
