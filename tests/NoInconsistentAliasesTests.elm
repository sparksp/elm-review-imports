module NoInconsistentAliasesTests exposing (all)

import NoInconsistentAliases as Rule exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoInconsistentAliases"
        [ test "reports incorrect aliases" <|
            \_ ->
                """
module Main exposing (main)
import Html
import Html.Attributes as A
main = Html.div [ A.class "container" ] []
"""
                    |> Review.Test.run
                        (Rule.config
                            [ ( [ "Html", "Attributes" ], "Attr" )
                            ]
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ incorrectAliasError "Attr" "Html.Attributes" "A"
                            |> Review.Test.atExactly { start = { row = 4, column = 27 }, end = { row = 4, column = 28 } }
                            |> Review.Test.whenFixed
                                """
module Main exposing (main)
import Html
import Html.Attributes as Attr
main = Html.div [ Attr.class "container" ] []
"""
                        ]
        , test "reports incorrect aliases in a function signature" <|
            \_ ->
                """
module Main exposing (main)
import Json.Encode as E
import Page
main : Program E.Value Page.Model Page.Msg
main =
    Page.program
"""
                    |> Review.Test.run
                        (Rule.config
                            [ ( [ "Json", "Encode" ], "Encode" )
                            ]
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ incorrectAliasError "Encode" "Json.Encode" "E"
                            |> Review.Test.atExactly { start = { row = 3, column = 23 }, end = { row = 3, column = 24 } }
                            |> Review.Test.whenFixed
                                """
module Main exposing (main)
import Json.Encode as Encode
import Page
main : Program Encode.Value Page.Model Page.Msg
main =
    Page.program
"""
                        ]
        , test "reports incorrect aliases in a type alias" <|
            \_ ->
                """
module Main exposing (main)
import Json.Encode as E
import Page
type alias JsonValue = E.Value
main : Program JsonValue Page.Model Page.Msg
main =
    Page.program
"""
                    |> Review.Test.run
                        (Rule.config
                            [ ( [ "Json", "Encode" ], "Encode" )
                            ]
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ incorrectAliasError "Encode" "Json.Encode" "E"
                            |> Review.Test.atExactly { start = { row = 3, column = 23 }, end = { row = 3, column = 24 } }
                            |> Review.Test.whenFixed
                                """
module Main exposing (main)
import Json.Encode as Encode
import Page
type alias JsonValue = Encode.Value
main : Program JsonValue Page.Model Page.Msg
main =
    Page.program
"""
                        ]
        , test "reports incorrect aliases in a custom type constructor" <|
            \_ ->
                """
module Main exposing (main)
import Json.Encode as E
import Page
type JsonValue = JsonValue E.Value
main = Page.main
"""
                    |> Review.Test.run
                        (Rule.config
                            [ ( [ "Json", "Encode" ], "Encode" )
                            ]
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ incorrectAliasError "Encode" "Json.Encode" "E"
                            |> Review.Test.atExactly { start = { row = 3, column = 23 }, end = { row = 3, column = 24 } }
                            |> Review.Test.whenFixed
                                """
module Main exposing (main)
import Json.Encode as Encode
import Page
type JsonValue = JsonValue Encode.Value
main = Page.main
"""
                        ]
        , test "reports incorrect aliases in case expressions" <|
            \_ ->
                """
module Visitor exposing (expressionVisitor)
import Elm.Syntax.Expression as ESE
import Elm.Syntax.Node as ESN
expressionVisitor : ESN.Node ESE.Expression -> List (Error {})
expressionVisitor node =
    case ESN.value node of
        ESE.FunctionOrValue _ _ ->
            []
        _ ->
            []
"""
                    |> Review.Test.run
                        (Rule.config
                            [ ( [ "Elm", "Syntax", "Expression" ], "Expression" )
                            , ( [ "Elm", "Syntax", "Node" ], "Node" )
                            ]
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ incorrectAliasError "Expression" "Elm.Syntax.Expression" "ESE"
                            |> Review.Test.atExactly { start = { row = 3, column = 33 }, end = { row = 3, column = 36 } }
                            |> Review.Test.whenFixed
                                """
module Visitor exposing (expressionVisitor)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Node as ESN
expressionVisitor : ESN.Node Expression.Expression -> List (Error {})
expressionVisitor node =
    case ESN.value node of
        Expression.FunctionOrValue _ _ ->
            []
        _ ->
            []
"""
                        , incorrectAliasError "Node" "Elm.Syntax.Node" "ESN"
                            |> Review.Test.atExactly { start = { row = 4, column = 27 }, end = { row = 4, column = 30 } }
                            |> Review.Test.whenFixed
                                """
module Visitor exposing (expressionVisitor)
import Elm.Syntax.Expression as ESE
import Elm.Syntax.Node as Node
expressionVisitor : Node.Node ESE.Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        ESE.FunctionOrValue _ _ ->
            []
        _ ->
            []
"""
                        ]
        , test "reports incorrect aliases in function arguments" <|
            \_ ->
                """
module Visitor exposing (getRange)
import Elm.Syntax.Node as ESN
getRange ((ESN.Node range _) as node) = range
"""
                    |> Review.Test.run
                        (Rule.config
                            [ ( [ "Elm", "Syntax", "Node" ], "Node" )
                            ]
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ incorrectAliasError "Node" "Elm.Syntax.Node" "ESN"
                            |> Review.Test.atExactly { start = { row = 3, column = 27 }, end = { row = 3, column = 30 } }
                            |> Review.Test.whenFixed
                                """
module Visitor exposing (getRange)
import Elm.Syntax.Node as Node
getRange ((Node.Node range _) as node) = range
"""
                        ]
        , test "reports incorrect aliases in let blocks" <|
            \_ ->
                """
module Visitor exposing (shiftRange)
import Elm.Syntax.Node as ESN
shiftRange : ( ESN.Node String, Int ) -> { node : ESN.Node String } -> { a | node : ESN.Node String } -> Range
shiftRange input _ _ =
    let
        asList : ( ESN.Node, Int ) -> List ESN.Node
        asList ( node, _ ) = [ node ]
        ( ESN.Node range1 value1, length1 ) = input
        [ ESN.Node range2 value2 ] = asList input
        ESN.Node range3 value3 :: [] = asList input
    in
    range
"""
                    |> Review.Test.run
                        (Rule.config
                            [ ( [ "Elm", "Syntax", "Node" ], "Node" )
                            ]
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ incorrectAliasError "Node" "Elm.Syntax.Node" "ESN"
                            |> Review.Test.atExactly { start = { row = 3, column = 27 }, end = { row = 3, column = 30 } }
                            |> Review.Test.whenFixed
                                """
module Visitor exposing (shiftRange)
import Elm.Syntax.Node as Node
shiftRange : ( Node.Node String, Int ) -> { node : Node.Node String } -> { a | node : Node.Node String } -> Range
shiftRange input _ _ =
    let
        asList : ( Node.Node, Int ) -> List Node.Node
        asList ( node, _ ) = [ node ]
        ( Node.Node range1 value1, length1 ) = input
        [ Node.Node range2 value2 ] = asList input
        Node.Node range3 value3 :: [] = asList input
    in
    range
"""
                        ]
        , test "reports incorrect aliases in a lambda function" <|
            \_ ->
                """
module NoCode exposing (visitor)
import Elm.Syntax.Node as ESN
import Review.Rule as Review
visitor : List (ESN.Node String) -> List (Review.Error {})
visitor list =
    List.map (\\ESN.Node range value -> Review.error value) list
"""
                    |> Review.Test.run
                        (Rule.config
                            [ ( [ "Elm", "Syntax", "Node" ], "Node" )
                            , ( [ "Review", "Rule" ], "Rule" )
                            ]
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ incorrectAliasError "Node" "Elm.Syntax.Node" "ESN"
                            |> Review.Test.atExactly { start = { row = 3, column = 27 }, end = { row = 3, column = 30 } }
                            |> Review.Test.whenFixed
                                """
module NoCode exposing (visitor)
import Elm.Syntax.Node as Node
import Review.Rule as Review
visitor : List (Node.Node String) -> List (Review.Error {})
visitor list =
    List.map (\\Node.Node range value -> Review.error value) list
"""
                        , incorrectAliasError "Rule" "Review.Rule" "Review"
                            |> Review.Test.atExactly { start = { row = 4, column = 23 }, end = { row = 4, column = 29 } }
                            |> Review.Test.whenFixed
                                """
module NoCode exposing (visitor)
import Elm.Syntax.Node as ESN
import Review.Rule as Rule
visitor : List (ESN.Node String) -> List (Rule.Error {})
visitor list =
    List.map (\\ESN.Node range value -> Rule.error value) list
"""
                        ]
        , test "does not report modules imported with no alias" <|
            \_ ->
                """
module Main exposing (main)
import Html.Attributes
main = 1"""
                    |> Review.Test.run
                        (Rule.config
                            [ ( [ "Html", "Attributes" ], "Attr" )
                            ]
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "does not report modules with the correct alias" <|
            \_ ->
                """
module Main exposing (main)
import Html.Attributes as Attr
main = 1"""
                    |> Review.Test.run
                        (Rule.config
                            [ ( [ "Html", "Attributes" ], "Attr" )
                            ]
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        , test "does not report modules with no preferred alias" <|
            \_ ->
                """
module Main exposing (main)
import Json.Encode as E
main = 1"""
                    |> Review.Test.run
                        (Rule.config
                            [ ( [ "Html", "Attributes" ], "Attr" )
                            ]
                            |> rule
                        )
                    |> Review.Test.expectNoErrors
        ]


incorrectAliasError : String -> String -> String -> Review.Test.ExpectedError
incorrectAliasError expectedAlias moduleName wrongAlias =
    Review.Test.error
        { message = "Incorrect alias `" ++ wrongAlias ++ "` for module `" ++ moduleName ++ "`"
        , details =
            [ "This import does not use your preferred alias `" ++ expectedAlias ++ "` for `" ++ moduleName ++ "`."
            , "You should update the alias to be consistent with the rest of the project. "
                ++ "Remember to change all references to the alias in this module too."
            ]
        , under = wrongAlias
        }
