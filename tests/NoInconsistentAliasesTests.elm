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
import Html.Attributes as A
main = 1"""
                    |> Review.Test.run
                        (Rule.config
                            [ ( [ "Html", "Attributes" ], "Attr" )
                            ]
                            |> rule
                        )
                    |> Review.Test.expectErrors
                        [ incorrectAliasError "Attr" "Html.Attributes" "A"
                            |> Review.Test.atExactly { start = { row = 3, column = 27 }, end = { row = 3, column = 28 } }
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
        , details = [ "This import does not use your preferred alias `" ++ expectedAlias ++ "`." ]
        , under = wrongAlias
        }
