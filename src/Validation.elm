module Validation
    exposing
        ( Field
        , Validator
        , Validity (..)
        , Event (..)
        , SubmissionStatus(..)
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

{-| This library helps with validation of input forms.

# Definition
@docs Field, Validity, Event, SubmissionStatus, OptionalField, Validator, ErrorMessage

# Helpers
@docs extractError, field, preValidatedField, validate, validity, rawValue, optional

# Higher-Order Helpers
@docs (|:), (>&&)

-}

{-| Define data type.
For example:

    dist = Field 40.5 NotValidated

That means `dist` has value 40.5 which hasn't validated yet.
-}
type Field raw a
    = Field raw (Validity a)


{-| This type defines three state of Field:
    * `NotValidated` values e.g. in input from, which have not validated yet.
    * `Valid a` values that they are correct.
    * `Invalid String` and state for incorrect input values.
-}
type Validity a
    = NotValidated
    | Valid a
    | Invalid String


{-| Event describe state of input form:
    * `OnSubmit` validates model data before submitting to server,
        see `validateModel` in `example`.
    * `OnBlur` validates input form when user leaves an input field.
    * `OnRelatedChange` validates input form which is tied with another form.
        For example: password and confirm form.
    * `OnChange raw` validates input form when user changes value in input field,
-}
type Event raw
    = OnSubmit
    | OnBlur
    | OnRelatedChange
    | OnChange raw


{-| Here `SubmissionStatus` define states for submit data to server:
    * `NotSubmitted` means that data have not sent yet.
    * `InProcess` for data being processed.
    * `Succeeded` if data have been successfully received.
    * `Failed` or vice versa, data have not been successfully received.

    This also may be used to inform user on screen, see `renderStatus`in `example`.
-}
type SubmissionStatus
    = NotSubmitted
    | InProcess
    | Succeeded
    | Failed


{-| Sometimes we want to use input form as optional, for example age.
In this case the input field can be an empty. But if somebody provides
input value then input field will be validated. So this type is used
for define optional variable.
-}
type alias OptionalField raw a =
    Field raw (Maybe a)


{-| It's used for validate variables, see `Validators` for example.
-}
type alias Validator a b =
    a -> Result String b


{-| Represents error message for invalid values in input form.
This type is used for definition of validator functions.
-}
type alias ErrorMessage =
    String


{-| Get value from Field.

    import Validation exposing (Field)

    intValue : Field String String
    intValue =
        Field "50" NotValidated

    rawValue intValue       -- "50"

-}
rawValue : Field b a -> b
rawValue (Field rawValue _) =
    rawValue


{-| Get validity from Field.

    import Validation exposing (Field)

    intValue : Field String Int
    intValue =
        Field "50" (Valid 50)

    validity intValue       -- Valid 50

-}
validity : Field raw a -> Validity a
validity (Field _ validity) =
    validity


{-| Default setting of Field with `NotValidated` validity.

    import Validation exposing (Field, field)

    intValue : Field String String
    intValue =
        field "50"      -- Field "50" NotValidated

-}
field : b -> Field b a
field value =
    Field value NotValidated


{-| Default setting of Field with `Valid a` validity.

    import Validation exposing (Field, field)

    intValue : Field String Int
    intValue =
        preValidatedField "50"      -- Field "50" (Valid 50)

-}
preValidatedField : b -> Field String a
preValidatedField value =
    Field (toString value) (Valid value)


{-| Run validation on Field with `Event`.
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


{-| Applying function to particular validation. For instance, see `submitIfValid` and
`submit` function in `example`.
-}
(|:) : Validity (a -> b) -> Validity a -> Validity b
(|:) =
    flip applyValidity


{-| Composition of two Validators.

    import Validators exposing (isNotEmpty, isEmail)

    emailValidation : Validator String String
    emailValidation =
        isNotEmpty "An email is required."
            >&& isEmail "Please ensure this is a valid email."

-}
(>&&) : Validator a b -> Validator b c -> Validator a c
(>&&) f g =
    f >> Result.andThen g


{-| Extract error message from Field.

    import Validaton exposing (Field)

    errorLabel : Field raw a -> Html Msg
    errorLabel field =
        div []
            [ field
                |> extractError
                |> Maybe.withDefault ""
                |> text
            ]

-}
extractError : Field raw a -> Maybe String
extractError field =
    case validity field of
        Invalid err ->
            Just err

        _ ->
            Nothing


{-| Validation of optional variable.

    import Validaton exposing (Event(..), OptionalField, field, validate)
    import Validators exposing (isPositiveInt)

    age : OptionalField String Int
    age =
        field ""

    validate
        OnSubmit
        (optional (isPositiveInt "The age has to be positive number."))
        age

    -- Field "" (Valid Nothing)

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
