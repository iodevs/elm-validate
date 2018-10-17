module TransformationTests exposing (suite)

import Expect
import Fuzz exposing (floatRange, int, intRange, string)
import Test exposing (..)
import Transformation exposing (toModel, withField, withoutField)
import Validation exposing (Field, preValidatedField)


suite : Test
suite =
    describe "The Transformation module"
        [ describe "transforms form to model with correct inputs"
            [ fuzz3 (intRange 1 100) (floatRange 0.1 99.9) string "should return 'Ok model'" <|
                \num real str ->
                    Form
                        (preValidatedField String.fromInt num)
                        (preValidatedField String.fromFloat real)
                        (preValidatedField identity str)
                        (C str)
                        |> toModel
                            Model
                            (withField condIntRange .a
                                >> withField condFloatRange .b
                                >> withField (\s -> Result.Ok (C s)) .c
                                >> withoutField Ok .d
                            )
                        |> Expect.equal (Ok { a = A num, b = B real, c = C str, d = C str })
            ]
        , describe "transforms form to model with incorrect inputs"
            [ fuzz3 (intRange -100 0) (floatRange -100 0) string "should return 'Err err'" <|
                \num real str ->
                    Form
                        (preValidatedField String.fromInt num)
                        (preValidatedField String.fromFloat real)
                        (preValidatedField identity str)
                        (C str)
                        |> toModel
                            Model
                            (withField condIntRange .a
                                >> withField condFloatRange .b
                                >> withField condString .c
                                >> withoutField Ok .d
                            )
                        |> expectErr
            ]
        ]


expectErr : Result error value -> Expect.Expectation
expectErr result =
    case result of
        Ok okVal ->
            Expect.fail ("Expected an Err but got " ++ Debug.toString result)

        Err _ ->
            Expect.pass


type A
    = A Int


type B
    = B Float


type C
    = C String


type alias Model =
    { a : A, b : B, c : C, d : C }


type alias Form =
    { a : Field String Int, b : Field String Float, c : Field String String, d : C }


condIntRange : Int -> Result String A
condIntRange val =
    if val > 0 then
        Ok (A val)

    else
        Err "Value must be positive number!"


condFloatRange : Float -> Result String B
condFloatRange val =
    if val < 100 && val > 0 then
        Ok (B val)

    else
        Err "Value isn't in range!"


condString : String -> Result String C
condString str =
    if str == "test" then
        Ok (C str)

    else
        Err "Value isn't 'text'!"
