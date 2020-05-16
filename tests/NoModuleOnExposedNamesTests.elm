module NoModuleOnExposedNamesTests exposing (all)

import NoModuleOnExposedNames exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoModuleOnExposedNames"
        [ test "reports modules used on exposed names" <|
            \_ ->
                """
module Page exposing (view)
import Html.Attributes as Attr exposing (class)
view children =
    div [ Attr.class "container" ] children
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ moduleOnExposedNameError "Attr.class" "class"
                            |> Review.Test.whenFixed
                                """
module Page exposing (view)
import Html.Attributes as Attr exposing (class)
view children =
    div [ class "container" ] children
"""
                        ]
        , test "does not report names not exposed" <|
            \_ ->
                """
module Page exposing (view)
import Html.Attributes as Attr
view children =
    div [ Attr.class "container" ] children
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]


moduleOnExposedNameError : String -> String -> Review.Test.ExpectedError
moduleOnExposedNameError call name =
    Review.Test.error
        { message = "Module used on exposed name `" ++ name ++ "`."
        , details =
            [ "It is not necessary to use the module here as `" ++ name ++ "` was exposed on import."
            , "You should remove the module from this call, or remove the name from the import .. exposing list."
            ]
        , under = call
        }
