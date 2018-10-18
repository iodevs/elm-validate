module TransformationTests exposing (suite)

import Expect
import Fuzz exposing (float, floatRange, int, intRange, string)
import Test exposing (..)
import Transformation exposing (toModel, withField, withoutField)
import Validation exposing (Field, preValidatedField)


suite : Test
suite =
    describe "The Transformation module"
        [ describe "transforms form to model"
            [ fuzz3 int float string "should return 'Result String Model'" <|
                \num real str ->
                    expectedModel num real str
            ]
        ]


expectedModel : Int -> Float -> String -> Expect.Expectation
expectedModel a b c =
    let
        form =
            Form
                (preValidatedField String.fromInt a)
                (preValidatedField String.fromFloat b)
                (preValidatedField identity c)
                (C c)
    in
    if 0 < a && 0 < b && b < 100 then
        form
            |> toModel
                Model
                (withField condIntRange .a
                    >> withField condFloatRange .b
                    >> withField (\s -> Result.Ok (C s)) .c
                    >> withoutField Ok .d
                )
            |> Expect.equal (Ok { a = A a, b = B b, c = C c, d = C c })

    else
        form
            |> toModel
                Model
                (withField condIntRange .a
                    >> withField condFloatRange .b
                    >> withField condString .c
                    >> withoutField Ok .d
                )
            |> expectErr


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
