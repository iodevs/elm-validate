module ValidatorsTests exposing (suite)

import Expect
import Fuzz exposing (Fuzzer, bool, constant, float, int, list, oneOf, string)
import Test exposing (..)
import Validation exposing (Field(..), Validity(..), preValidatedField)
import Validators exposing (..)


suite : Test
suite =
    describe "is now verifying the function"
        [ describe "isFloat, which"
            [ fuzz (oneOf [ Fuzz.map String.fromFloat float, string ]) "should return float validator result" <|
                \val ->
                    let
                        result =
                            isFloat "ERROR" val
                    in
                    case result of
                        Ok v ->
                            Expect.equal (Ok v) result

                        Err _ ->
                            Expect.equal (Err "ERROR") result
            ]
        , describe "isPositiveFloat, which"
            [ fuzz float "should return positive float validator result" <|
                \num ->
                    let
                        result =
                            num
                                |> String.fromFloat
                                |> isPositiveFloat "ERROR"
                    in
                    if 0 < num then
                        Expect.equal (Ok num) result

                    else
                        Expect.equal (Err "ERROR") result
            ]
        , describe "isInt, which"
            [ fuzz (oneOf [ Fuzz.map String.fromInt int, string ]) "should return int validator result" <|
                \val ->
                    let
                        result =
                            isInt "ERROR" val
                    in
                    case result of
                        Ok v ->
                            Expect.equal (Ok v) result

                        Err _ ->
                            Expect.equal (Err "ERROR") result
            ]
        , describe "isPositiveInt, which"
            [ fuzz int "should return positive int validator result" <|
                \num ->
                    let
                        result =
                            num
                                |> String.fromInt
                                |> isPositiveInt "ERROR"
                    in
                    if 0 < num then
                        Expect.equal (Ok num) result

                    else
                        Expect.equal (Err "ERROR") result
            ]
        , describe "isTrue, which"
            [ fuzz bool "should return true validator result" <|
                \bool ->
                    let
                        result =
                            isTrue "ERROR" bool
                    in
                    if bool then
                        Expect.equal (Ok True) result

                    else
                        Expect.equal (Err "ERROR") result
            ]
        , describe "isEqualTo, which"
            [ fuzz (oneOf [ string, constant "foo" ]) "should return validator result" <|
                \str ->
                    let
                        field =
                            if str == "foo" then
                                Field "" (Valid "bar")

                            else
                                Field "" (Valid str)

                        result =
                            isEqualTo field "ERROR" str
                    in
                    if str == "foo" then
                        Expect.equal (Err "ERROR") result

                    else
                        Expect.equal (Ok str) result
            ]
        , describe "isNotEmpty, which"
            [ fuzz string "should return nonempty validator result" <|
                \str ->
                    let
                        result =
                            isNotEmpty "ERROR" str
                    in
                    if String.isEmpty str then
                        Expect.equal (Err "ERROR") result

                    else
                        Expect.equal (Ok str) result
            ]
        , describe "isInList, which"
            [ fuzz2 float (list float) "should return validator result" <|
                \val list ->
                    let
                        result =
                            isInList "ERROR" ( val, list )
                    in
                    if List.member val list then
                        Expect.equal (Ok val) result

                    else
                        Expect.equal (Err "ERROR") result
            ]
        , describe "isValidField, which"
            [ fuzz float "should return bool" <|
                \num ->
                    let
                        field =
                            if num < 0 then
                                Field "" NotValidated

                            else if 0 < num && num < 10 then
                                Field "" (Invalid "foo")

                            else
                                Field "" (Valid num)

                        result =
                            isValidField field
                    in
                    if result then
                        Expect.true "Field is valid." result

                    else
                        Expect.false "Field is not valid." result
            ]
        ]
