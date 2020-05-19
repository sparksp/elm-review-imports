module NoModuleOnExposedNamesTests exposing (all)

import NoModuleOnExposedNames exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoModuleOnExposedNames"
        [ test "reports modules used on exposed values" <|
            \_ ->
                """
module Page exposing (view)
import Html.Attributes as Attr exposing (class)
view children =
    div [ Attr.class "container" ] children
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ moduleOnExposedValueError "Attr.class" "class"
                            |> Review.Test.whenFixed
                                """
module Page exposing (view)
import Html.Attributes as Attr exposing (class)
view children =
    div [ class "container" ] children
"""
                        ]
        , test "reports modules used on exposed types" <|
            \_ ->
                """
module Page exposing (view)
import Html exposing (Html, Attribute)
view : List (Html.Attribute msg) -> Html msg
view children =
    Html.div [] children
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ moduleOnExposedTypeError "Html.Attribute" "Attribute"
                            |> Review.Test.whenFixed
                                """
module Page exposing (view)
import Html exposing (Html, Attribute)
view : List (Attribute msg) -> Html msg
view children =
    Html.div [] children
"""
                        ]
        , test "reports modules used on default imports" <|
            \_ ->
                """
module Page exposing (view)
import Html exposing (Html)
import Html.Attributes as Attr
view : Maybe.Maybe (String.String) -> Html msg
view maybeClass =
    case maybeClass of
        Maybe.Just class ->
            Html.div [ Attr.class class ] []

        _ ->
            Html.div [] [ Html.text (String.fromFloat Basics.pi) ]
"""
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ moduleOnDefaultTypeError "Maybe.Maybe" "Maybe"
                            |> Review.Test.whenFixed
                                """
module Page exposing (view)
import Html exposing (Html)
import Html.Attributes as Attr
view : Maybe (String.String) -> Html msg
view maybeClass =
    case maybeClass of
        Maybe.Just class ->
            Html.div [ Attr.class class ] []

        _ ->
            Html.div [] [ Html.text (String.fromFloat Basics.pi) ]
"""
                        , moduleOnDefaultTypeError "String.String" "String"
                            |> Review.Test.whenFixed
                                """
module Page exposing (view)
import Html exposing (Html)
import Html.Attributes as Attr
view : Maybe.Maybe (String) -> Html msg
view maybeClass =
    case maybeClass of
        Maybe.Just class ->
            Html.div [ Attr.class class ] []

        _ ->
            Html.div [] [ Html.text (String.fromFloat Basics.pi) ]
"""
                        , moduleOnDefaultValueError "Maybe.Just" "Just"
                            |> Review.Test.whenFixed
                                """
module Page exposing (view)
import Html exposing (Html)
import Html.Attributes as Attr
view : Maybe.Maybe (String.String) -> Html msg
view maybeClass =
    case maybeClass of
        Just class ->
            Html.div [ Attr.class class ] []

        _ ->
            Html.div [] [ Html.text (String.fromFloat Basics.pi) ]
"""
                        , moduleOnDefaultValueError "Basics.pi" "pi"
                            |> Review.Test.whenFixed
                                """
module Page exposing (view)
import Html exposing (Html)
import Html.Attributes as Attr
view : Maybe.Maybe (String.String) -> Html msg
view maybeClass =
    case maybeClass of
        Maybe.Just class ->
            Html.div [ Attr.class class ] []

        _ ->
            Html.div [] [ Html.text (String.fromFloat pi) ]
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


moduleOnExposedValueError : String -> String -> Review.Test.ExpectedError
moduleOnExposedValueError call name =
    Review.Test.error
        { message = "Module used on exposed value `" ++ name ++ "`."
        , details =
            [ "It is not necessary to use the module here as `" ++ name ++ "` was exposed on import."
            , "You should remove the module from this call, or remove the name from the import .. exposing list."
            ]
        , under = call
        }


moduleOnExposedTypeError : String -> String -> Review.Test.ExpectedError
moduleOnExposedTypeError call name =
    Review.Test.error
        { message = "Module used on exposed type `" ++ name ++ "`."
        , details =
            [ "It is not necessary to use the module here as `" ++ name ++ "` was exposed on import."
            , "You should remove the module from this call, or remove the name from the import .. exposing list."
            ]
        , under = call
        }


moduleOnDefaultValueError : String -> String -> Review.Test.ExpectedError
moduleOnDefaultValueError call name =
    Review.Test.error
        { message = "Module used on exposed value `" ++ name ++ "`."
        , details =
            [ "It is not necessary to use the module here as Elm exposes `" ++ name ++ "` by default. You should remove the module from this call."
            , "You can see the full list of default imports at: https://package.elm-lang.org/packages/elm/core/latest/#default-imports"
            ]
        , under = call
        }


moduleOnDefaultTypeError : String -> String -> Review.Test.ExpectedError
moduleOnDefaultTypeError call name =
    Review.Test.error
        { message = "Module used on exposed type `" ++ name ++ "`."
        , details =
            [ "It is not necessary to use the module here as Elm exposes `" ++ name ++ "` by default. You should remove the module from this call."
            , "You can see the full list of default imports at: https://package.elm-lang.org/packages/elm/core/latest/#default-imports"
            ]
        , under = call
        }
