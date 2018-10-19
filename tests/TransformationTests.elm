module TransformationTests exposing (suite)

import Expect
import Fuzz exposing (constant, float, floatRange, int, intRange, oneOf, string)
import Test exposing (..)
import Transformation exposing (toModel, withField, withoutField)
import Validation exposing (Field, preValidatedField)


suite : Test
suite =
    describe "The Transformation module"
        [ describe "transforms form to model"
            [ fuzz3 int float (oneOf [ string, constant "test" ]) "should return 'Result String Model'" <|
                \num real str ->
                    let
                        form =
                            Form
                                (preValidatedField String.fromInt num)
                                (preValidatedField String.fromFloat real)
                                (preValidatedField identity str)
                                (C str)

                        result =
                            form
                                |> toModel
                                    Model
                                    (withField condIntRange .a
                                        >> withField condFloatRange .b
                                        >> withField condString .c
                                        >> withoutField Ok .d
                                    )
                    in
                    Expect.equal (expectedModel num real str str) result
            ]
        ]


expectedModel : Int -> Float -> String -> String -> Result String Model
expectedModel a b c d =
    Result.map4
        Model
        (condIntRange a)
        (condFloatRange b)
        (condString c)
        (Ok (C d))


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
