module Elm.DefaultImports exposing (isModuleImported, isTypeExposed, isValueExposed)

import Elm.Interface as Interface exposing (Interface)
import Elm.Syntax.ModuleName exposing (ModuleName)


isModuleImported : ModuleName -> Bool
isModuleImported moduleName =
    interfaceForModule moduleName /= Nothing


isTypeExposed : ModuleName -> String -> Bool
isTypeExposed moduleName typeOrAlias =
    case interfaceForModule moduleName of
        Nothing ->
            False

        Just interface ->
            interfaceExposesTypeOrAlias typeOrAlias interface


isValueExposed : ModuleName -> String -> Bool
isValueExposed moduleName function =
    case interfaceForModule moduleName of
        Nothing ->
            False

        Just interface ->
            Interface.exposesFunction function interface



--- INTERFACE


interfaceForModule : ModuleName -> Maybe Interface
interfaceForModule moduleName =
    case moduleName of
        [ "Basics" ] ->
            Just
                [ Interface.CustomType ( "Int", [] )
                , Interface.CustomType ( "Float", [] )
                , Interface.Function "toFloat"
                , Interface.Function "round"
                , Interface.Function "floor"
                , Interface.Function "ceiling"
                , Interface.Function "truncate"
                , Interface.Function "max"
                , Interface.Function "min"
                , Interface.Function "compare"
                , Interface.CustomType ( "Order", [ "LT", "EQ", "GT" ] )
                , Interface.CustomType ( "Bool", [ "True", "False" ] )
                , Interface.Function "not"
                , Interface.Function "xor"
                , Interface.Function "modBy"
                , Interface.Function "remainderBy"
                , Interface.Function "negate"
                , Interface.Function "abs"
                , Interface.Function "clamp"
                , Interface.Function "sqrt"
                , Interface.Function "logBase"
                , Interface.Function "e"
                , Interface.Function "degrees"
                , Interface.Function "radians"
                , Interface.Function "turns"
                , Interface.Function "pi"
                , Interface.Function "cos"
                , Interface.Function "sin"
                , Interface.Function "tan"
                , Interface.Function "acos"
                , Interface.Function "asin"
                , Interface.Function "atan"
                , Interface.Function "atan2"
                , Interface.Function "toPolar"
                , Interface.Function "fromPolar"
                , Interface.Function "isNaN"
                , Interface.Function "isInfinite"
                , Interface.Function "identity"
                , Interface.Function "always"
                , Interface.CustomType ( "Never", [] )
                , Interface.Function "never"
                ]

        [ "Char" ] ->
            Just
                [ Interface.CustomType ( "Char", [] )
                ]

        [ "Cmd" ] ->
            Just
                [ Interface.CustomType ( "Cmd", [] )
                ]

        [ "Debug" ] ->
            Just []

        [ "List" ] ->
            Just
                [ Interface.CustomType ( "List", [] )
                ]

        [ "Maybe" ] ->
            Just
                [ Interface.CustomType ( "Maybe", [ "Just", "Nothing" ] )
                ]

        [ "Platform" ] ->
            Just
                [ Interface.CustomType ( "Program", [] )
                ]

        [ "Result" ] ->
            Just
                [ Interface.CustomType ( "Result", [ "Ok", "Err" ] )
                ]

        [ "String" ] ->
            Just
                [ Interface.CustomType ( "String", [] )
                ]

        [ "Sub" ] ->
            Just
                [ Interface.CustomType ( "Sub", [] )
                ]

        [ "Tuple" ] ->
            Just []

        _ ->
            Nothing


interfaceExposesTypeOrAlias : String -> Interface -> Bool
interfaceExposesTypeOrAlias search interface =
    interface
        |> List.any
            (\entry ->
                case entry of
                    Interface.Alias name ->
                        name == search

                    Interface.CustomType ( name, _ ) ->
                        name == search

                    _ ->
                        False
            )
