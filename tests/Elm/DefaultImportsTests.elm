module Elm.DefaultImportsTests exposing (all)

import Elm.DefaultImports as DefaultImports
import Expect
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Elm.DefaultImports"
        [ testImportsModule "Basics"
        , testExposesType "Basics" "Int"
        , testExposesType "Basics" "Float"
        , testExposesValue "Basics" "toFloat"
        , testExposesValue "Basics" "round"
        , testExposesValue "Basics" "floor"
        , testExposesValue "Basics" "ceiling"
        , testExposesValue "Basics" "truncate"
        , testExposesValue "Basics" "max"
        , testExposesValue "Basics" "min"
        , testExposesValue "Basics" "compare"
        , testExposesType "Basics" "Order"
        , testExposesValue "Basics" "LT"
        , testExposesValue "Basics" "EQ"
        , testExposesValue "Basics" "GT"
        , testExposesType "Basics" "Bool"
        , testExposesValue "Basics" "True"
        , testExposesValue "Basics" "False"
        , testExposesValue "Basics" "not"
        , testExposesValue "Basics" "modBy"
        , testExposesValue "Basics" "remainderBy"
        , testExposesValue "Basics" "negate"
        , testExposesValue "Basics" "abs"
        , testExposesValue "Basics" "clamp"
        , testExposesValue "Basics" "sqrt"
        , testExposesValue "Basics" "logBase"
        , testExposesValue "Basics" "e"
        , testExposesValue "Basics" "degrees"
        , testExposesValue "Basics" "radians"
        , testExposesValue "Basics" "turns"
        , testExposesValue "Basics" "pi"
        , testExposesValue "Basics" "cos"
        , testExposesValue "Basics" "sin"
        , testExposesValue "Basics" "tan"
        , testExposesValue "Basics" "acos"
        , testExposesValue "Basics" "asin"
        , testExposesValue "Basics" "atan"
        , testExposesValue "Basics" "atan2"
        , testExposesValue "Basics" "toPolar"
        , testExposesValue "Basics" "fromPolar"
        , testExposesValue "Basics" "isNaN"
        , testExposesValue "Basics" "isInfinite"
        , testExposesValue "Basics" "identity"
        , testExposesValue "Basics" "always"
        , testExposesType "Basics" "Never"
        , testExposesValue "Basics" "never"
        , testImportsModule "List"
        , testExposesType "List" "List"
        , testImportsModule "Maybe"
        , testExposesType "Maybe" "Maybe"
        , testExposesValue "Maybe" "Just"
        , testExposesValue "Maybe" "Nothing"
        , testImportsModule "Result"
        , testExposesType "Result" "Result"
        , testExposesValue "Result" "Ok"
        , testExposesValue "Result" "Err"
        , testImportsModule "String"
        , testExposesType "String" "String"
        , testImportsModule "Char"
        , testExposesType "Char" "Char"
        , testImportsModule "Tuple"
        , testImportsModule "Debug"
        , testImportsModule "Platform"
        , testExposesType "Platform" "Program"
        , testImportsModule "Cmd"
        , testExposesType "Cmd" "Cmd"
        , testImportsModule "Sub"
        , testExposesType "Sub" "Sub"
        ]


testImportsModule : String -> Test
testImportsModule moduleName =
    test ("importsModule: " ++ moduleName) <|
        \_ ->
            DefaultImports.isModuleImported [ moduleName ]
                |> Expect.true "Expected module to be imported."


testExposesValue : String -> String -> Test
testExposesValue moduleName value =
    test ("exposesValue: " ++ moduleName ++ "." ++ value) <|
        \_ ->
            DefaultImports.isValueExposed [ moduleName ] value
                |> Expect.true "Expected value to be exposed."


testExposesType : String -> String -> Test
testExposesType moduleName name =
    test ("exposesType: " ++ moduleName ++ "." ++ name) <|
        \_ ->
            DefaultImports.isTypeExposed [ moduleName ] name
                |> Expect.true "Expected type or alias to be exposed."
