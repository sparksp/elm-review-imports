module Tests.NoModuleOnExposedNames.Context exposing (all)

import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range as Range
import Expect
import NoModuleOnExposedNames.Context as Context
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoModuleOnExposedNames.Context"
        [ describe "isTypeExposed"
            [ test "is False with no imports" <|
                \() ->
                    Context.isTypeExposed Context.initial [ "Html" ] "Attribute"
                        |> Expect.equal False
            , test "is True with Exposing.All" <|
                \() ->
                    let
                        context : Context.Module
                        context =
                            Context.expose [ "Html" ] (Exposing.All Range.emptyRange) Context.initial
                    in
                    Context.isTypeExposed context [ "Html" ] "Attribute"
                        |> Expect.equal True
            , test "is True with matching TypeExpose" <|
                \() ->
                    let
                        context : Context.Module
                        context =
                            Context.expose [ "Html" ] (Exposing.Explicit [ typeExpose "Attribute" ]) Context.initial
                    in
                    Context.isTypeExposed context [ "Html" ] "Attribute"
                        |> Expect.equal True
            , test "is True with matching TypeOrAliasExpose" <|
                \() ->
                    let
                        context : Context.Module
                        context =
                            Context.expose [ "Html" ] (Exposing.Explicit [ typeOrAliasExpose "Attribute" ]) Context.initial
                    in
                    Context.isTypeExposed context [ "Html" ] "Attribute"
                        |> Expect.equal True
            , test "is False with matching FunctionExpose" <|
                \() ->
                    let
                        context : Context.Module
                        context =
                            Context.expose [ "Html" ] (Exposing.Explicit [ functionExpose "div" ]) Context.initial
                    in
                    -- Functions are not types
                    Context.isTypeExposed context [ "Html" ] "div"
                        |> Expect.equal False
            , test "is False with no matching TypeExpose" <|
                \() ->
                    let
                        context : Context.Module
                        context =
                            Context.expose [ "Html" ] (Exposing.Explicit [ typeExpose "Html" ]) Context.initial
                    in
                    Context.isTypeExposed context [ "Html" ] "Attribute"
                        |> Expect.equal False
            ]
        ]


functionExpose : String -> Node Exposing.TopLevelExpose
functionExpose name =
    Node Range.emptyRange (Exposing.FunctionExpose name)


typeExpose : String -> Node Exposing.TopLevelExpose
typeExpose name =
    Node Range.emptyRange (Exposing.TypeExpose { name = name, open = Nothing })


typeOrAliasExpose : String -> Node Exposing.TopLevelExpose
typeOrAliasExpose name =
    Node Range.emptyRange (Exposing.TypeOrAliasExpose name)
