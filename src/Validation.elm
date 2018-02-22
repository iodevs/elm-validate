module Validation
    exposing
        ( Field
        , Validator
        , Validity (..)
        , Event (..)
        , OptionalField
        , ErrorMessage
        , extractError
        , field
        , preValidatedField
        , validate
        , validity
        , rawValue
        , optional
        , (|:)
        , (>&&)
        )

{-| TODO: Description and examples

# Definition
@docs Field, Validator, Validity, Event, OptionalField, ErrorMessage

# Helpers
@docs extractError, field, preValidatedField, validate, validity, rawValue, optional

# Higher-Order Helpers
@docs (|:), (>&&)

-}


{-|
-}
type Validity a
    = NotValidated
    | Valid a
    | Invalid String


{-|
-}
type Event raw
    = OnSubmit
    | OnBlur
    | OnRelatedChange
    | OnChange raw


{-|
-}
type Field raw a
    = Field raw (Validity a)


{-|
-}
type alias OptionalField raw a =
    Field raw (Maybe a)


{-|
-}
type alias Validator a b =
    a -> Result String b


{-|
-}
type alias ErrorMessage =
    String


{-|
-}
rawValue : Field b a -> b
rawValue (Field rawValue _) =
    rawValue


{-|
-}
validity : Field raw a -> Validity a
validity (Field _ validity) =
    validity


{-|
-}
field : b -> Field b a
field value =
    Field value NotValidated


{-|
-}
preValidatedField : b -> Field String a
preValidatedField value =
    Field (toString value) (Valid value)


{-|
-}
validate : Event raw -> Validator raw a -> Field raw a -> Field raw a
validate event validate (Field value validity) =
    case event of
        OnSubmit ->
            validateAlways validate (Field value validity)

        OnBlur ->
            validateAlways validate (Field value validity)

        OnChange newValue ->
            validateIfValidated validate (Field newValue validity)

        OnRelatedChange ->
            validateIfValidated validate (Field value validity)


{-| Flip the order of the first two arguments to a function. -}
(|:) : Validity (a -> b) -> Validity a -> Validity b
(|:) =
    flip applyValidity


{-|
-}
(>&&) : Validator a b -> Validator b c -> Validator a c
(>&&) f g =
    f >> Result.andThen g


{-|
-}
extractError : Field raw a -> Maybe String
extractError field =
    case validity field of
        Invalid err ->
            Just err

        _ ->
            Nothing


{-|
-}
optional : Validator String a -> Validator String (Maybe a)
optional validate s =
    if String.isEmpty s then
        Ok Nothing
    else
        validate s |> Result.map Just



-- Internal function

validateAlways : Validator raw a -> Field raw a -> Field raw a
validateAlways validate (Field value validity) =
    Field value (validate value |> toValidity)


validateIfValidated : Validator raw a -> Field raw a -> Field raw a
validateIfValidated validate (Field value validity) =
    Field value
        (case validity of
            NotValidated ->
                NotValidated

            _ ->
                validate value |> toValidity
        )


toValidity : Result String a -> Validity a
toValidity result =
    case result of
        Ok a ->
            Valid a

        Err err ->
            Invalid err


applyValidity : Validity a -> Validity (a -> b) -> Validity b
applyValidity fa ff =
    case fa of
        NotValidated ->
            NotValidated

        Invalid err ->
            Invalid err

        Valid a ->
            case ff of
                NotValidated ->
                    NotValidated

                Invalid err ->
                    Invalid err

                Valid f ->
                    f a |> Valid
