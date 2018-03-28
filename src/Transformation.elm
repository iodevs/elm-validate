module Transformation
    exposing
        ( field
        , withoutField
        , toModel
        )

{-| This module helps to transform validated forms to models.


# Helpers

@docs field, withoutField, toModel

-}

import Validation
    exposing
        ( Field
        , Validity(..)
        , validity
        )


{-| It defines input data as Result.
-}
type Transformer data form
    = Transformer (Result String data) form


type alias Creator a b =
    a -> Result String b


type alias Accessor form raw a =
    form -> Field raw a


{-| This function is used to check if the given variable in the form has correct validity and
if yes, then returns this variable with her value. Otherwise returns an `Err errorMessage`.

    import Validation exposing (preValidatedField, Field)

    type A = A Int
    type B = B Float

    type alias Model = { a : A, b : B}
    type alias Form = { a : Field String Int, b : Field String Float}

    form = Form (preValidatedField 1) (preValidatedField 0.3)

    condPosInt : Int -> Result String A
    condPosInt val =
        if val > 0 then
            Ok (A val)
        else
            Err "Value must be positive number!!!"

    condFloatRange : Float -> Result String B
    condFloatRange val =
        if val < 10 && val > 0 then
            Ok (B val)
        else
            Err "Value isn't in range!!!"

    model : Result String Model
    model =
        toModel
            Model
            ( field condPosInt .a
            >> field condFloatRange .b
            )
            form

    -- Ok { a = 1, b = 0.3 }

-}
field :
    (a -> Result String b)
    -> (form -> Field raw a)
    -> Transformer (b -> c) form
    -> Transformer c form
field creator acs (Transformer model form) =
    case model of
        Ok mdl ->
            case validity (acs form) of
                NotValidated ->
                    Transformer (Err "Form is invalid!!!") form

                Invalid _ ->
                    Transformer (Err "Form is invalid!!!") form

                Valid val ->
                    Transformer
                        (val
                            |> creator
                            |> Result.map mdl
                        )
                        form

        Err msg ->
            Transformer (Err msg) form


{-| This function is similar to function above and is used for non-field type of variables.

    import Validation exposing (preValidatedField, Field)

    type Planet = Venus | Earth | Mars
    type alias Model = { planet : Planet, a: Int, b: Float}
    type alias Form = { planet : Planet, a : Field String Int, b : Field String Float}

    form = Form Earth (preValidatedField 1) (preValidatedField 0.3)
    --form = Form Mars (preValidatedField 1) (Validation.field "40.5")

    model : Result String Model
    model =
        let
            fieldOk = field Ok
            valueOk = withoutField Ok
        in
            toModel
                Model
                ( valueOk .planet
                >> fieldOk .a
                >> fieldOk .b
                )
                form

    -- Ok { planet = Earth, a = 1, b = 0.3 }
    -- for form with Mars we get:
    --      Err "Form is invalid!!!",
    -- because validity of initial value b is NotValidated.

-}
withoutField :
    (a -> Result String b)
    -> (form -> a)
    -> Transformer (b -> c) form
    -> Transformer c form
withoutField creator acs (Transformer model form) =
    case model of
        Ok mdl ->
            Transformer
                ((acs form)
                    |> creator
                    |> Result.map mdl
                )
                form

        Err msg ->
            Transformer (Err msg) form


{-| This function transforms form to model and returns an `Err errorMessage` if
any variable in the form is NotValidated either Invalid. Otherwise returns `Ok model`.
See examples above.
-}
toModel : model -> (Transformer model form -> Transformer data form) -> form -> Result String data
toModel model f form =
    let
        (Transformer result _) =
            f (Transformer (Ok model) form)
    in
        result
