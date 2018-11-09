module ValidationTests exposing (suite)

import Expect
import Fuzz exposing (Fuzzer, bool, constant, float, int, list, oneOf, string)
import Test exposing (..)
import Validation exposing (..)
import Validators exposing (..)


suite : Test
suite =
    describe "is now verifying the function"
        [ describe "rawValue, which"
            [ fuzz string "should return raw value from Field" <|
                \raw ->
                    let
                        fields =
                            [ Field raw NotValidated
                            , Field raw (Valid raw)
                            , Field raw (Invalid "invalid")
                            ]

                        results =
                            fields
                                |> List.map (\f -> rawValue f == raw)
                    in
                    Expect.true "Expected True." (List.foldl (&&) True results)
            ]
        , describe "validity, which"
            [ fuzz string "should return validity from Field" <|
                \raw ->
                    let
                        fields =
                            [ ( Field raw NotValidated, NotValidated )
                            , ( Field raw (Valid raw), Valid raw )
                            , ( Field raw (Invalid "invalid"), Invalid "invalid" )
                            ]

                        results =
                            fields
                                |> List.map (\( f, expected ) -> validity f == expected)
                    in
                    Expect.true "Expected True." (List.foldl (&&) True results)
            ]
        , describe "field, which"
            [ fuzz float "should return default `Float` settings of Field with `NotValidated` validity" <|
                \raw ->
                    Expect.all
                        [ \field -> Expect.within (Expect.Absolute 0.000000001) (rawValue field) raw
                        , \field -> Expect.equal (validity field) NotValidated
                        ]
                        (field raw)
            , fuzz string "should return default `String` settings of Field with `NotValidated` validity" <|
                \raw ->
                    Expect.all
                        [ \field -> Expect.equal (rawValue field) raw
                        , \field -> Expect.equal (validity field) NotValidated
                        ]
                        (field raw)
            ]
        , describe "preValidatedField, which"
            [ fuzz float "should return default settings of Field with `Valid Float` validity" <|
                \raw ->
                    Expect.all
                        [ \field -> Expect.equal (rawValue field) (String.fromFloat raw)
                        , \field -> Expect.equal (validity field) (Valid raw)
                        ]
                        (preValidatedField String.fromFloat raw)
            , fuzz string "should return default settings of Field with `Valid String` validity" <|
                \raw ->
                    Expect.all
                        [ \field -> Expect.equal (rawValue field) raw
                        , \field -> Expect.equal (validity field) (Valid raw)
                        ]
                        (preValidatedField identity raw)
            ]
        , describe "invalidate, which"
            [ fuzz float "should return Field with `Invalid String` validity" <|
                \raw ->
                    let
                        fields =
                            [ Field raw NotValidated
                            , Field raw (Valid raw)
                            , Field raw (Invalid "invalid")
                            ]

                        expected =
                            Field raw (Invalid "ERROR")

                        results =
                            fields
                                |> List.map (\f -> invalidate "ERROR" f == expected)
                    in
                    Expect.true "Expected True." (List.foldl (&&) True results)
            ]
        , describe "validate, which"
            [ fuzz int "should validate Field with `Event`" <|
                \val ->
                    let
                        raw =
                            String.fromInt val

                        expField =
                            if 0 < val then
                                Field raw (Valid val)

                            else
                                Field raw (Invalid "ERROR")

                        validator =
                            isGreaterThan (isInt "ERR") 0 "ERROR"

                        eventVal =
                            [ ( OnSubmit, Field raw NotValidated, expField )
                            , ( OnSubmit, Field raw (Valid val), expField )
                            , ( OnSubmit, Field raw (Invalid "ERROR2"), expField )
                            , ( OnBlur, Field raw NotValidated, expField )
                            , ( OnBlur, Field raw (Valid val), expField )
                            , ( OnBlur, Field raw (Invalid "ERROR2"), expField )
                            , ( OnChange raw, Field "10" NotValidated, Field raw NotValidated )
                            , ( OnChange raw, Field "10" (Valid 10), expField )
                            , ( OnChange raw, Field "10" (Invalid "ERROR2"), expField )
                            , ( OnRelatedChange, Field raw NotValidated, Field raw NotValidated )
                            , ( OnRelatedChange, Field raw (Valid val), expField )
                            , ( OnRelatedChange, Field raw (Invalid "ERROR2"), expField )
                            ]

                        results =
                            eventVal
                                |> List.map
                                    (\( event, field_, expected ) ->
                                        validate event validator field_ == expected
                                    )
                    in
                    Expect.true "Expected True." (List.foldl (&&) True results)
            ]
        , describe "applyValidity, which"
            [ test "should validate a pair of `Validity a`" <|
                \_ ->
                    let
                        applyVal =
                            [ ( NotValidated, NotValidated, NotValidated )
                            , ( NotValidated, Valid identity, NotValidated )
                            , ( NotValidated, Invalid "invalid", NotValidated )
                            , ( Valid "raw", NotValidated, NotValidated )
                            , ( Valid "raw", Valid identity, Valid "raw" )
                            , ( Valid "raw", Invalid "invalid", Invalid "invalid" )
                            , ( Invalid "invalid", NotValidated, Invalid "invalid" )
                            , ( Invalid "invalid", Valid identity, Invalid "invalid" )
                            , ( Invalid "invalid", Invalid "invalid", Invalid "invalid" )
                            ]

                        results =
                            applyVal
                                |> List.map (\( v1, v2, expected ) -> applyValidity v1 v2 == expected)
                    in
                    Expect.true "Expected True." (List.foldl (&&) True results)
            ]
        , describe "composite, which"
            [ fuzz (oneOf [ string, constant "", constant "test@test.com" ]) "should return validator result" <|
                \str ->
                    let
                        result =
                            composite (isNotEmpty "ERROR1") (isEmail "ERROR2") str
                    in
                    case result of
                        Ok v ->
                            Expect.equal v str

                        Err err ->
                            Expect.true "Expected True." (err == "ERROR1" || err == "ERROR2")
            ]
        , describe "extractError, which"
            [ fuzz string "should return error message from Field" <|
                \raw ->
                    let
                        fields =
                            [ ( Field raw NotValidated, Nothing )
                            , ( Field raw (Valid raw), Nothing )
                            , ( Field raw (Invalid "invalid"), Just "invalid" )
                            ]

                        results =
                            fields
                                |> List.map (\( f, expected ) -> extractError f == expected)
                    in
                    Expect.true "Expected True." (List.foldl (&&) True results)
            ]
        , describe "optional, which"
            [ fuzz (oneOf [ Fuzz.map String.fromInt int, constant "" ]) "should return result of Optional Field validation" <|
                \raw ->
                    let
                        result =
                            optional (isGreaterThan (isInt "ERR") 0 "ERROR") raw
                    in
                    case result of
                        Ok val ->
                            case val of
                                Just v ->
                                    Expect.true "Expected True." (String.fromInt v == raw)

                                Nothing ->
                                    Expect.pass

                        Err err ->
                            Expect.equal err "ERROR"
            ]
        ]
