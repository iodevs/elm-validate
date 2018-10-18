module Validators exposing
    ( isFloat, isPositiveFloat, isInt, isPositiveInt
    , isNotEmpty, isEmail, isUrl
    , isInList
    , isTrue, isEqualTo, isValidField
    )

{-| This library provides a few functions for validating data.


# Numbers validation

@docs isFloat, isPositiveFloat, isInt, isPositiveInt


# Strings validation

@docs isNotEmpty, isEmail, isUrl


# List validation

@docs isInList


# Others validation

@docs isTrue, isEqualTo, isValidField

-}

import Regex
import Validation
    exposing
        ( ErrorMessage
        , Field(..)
        , Validator
        , Validity(..)
        , composite
        , validity
        )


{-| Return an `Err errorMessage` if the given value isn't float number, otherwise
return `Ok value`.

    import Validation exposing (Validator, ErrorMessage)

    floatValidation : Validator String Float
    floatValidation =
        isFloat "It is not float number!"

    floatValidation "5.39161" -- Ok 5.39161
    floatValidation "t.39161" -- Err "It is not float number!"

-}
isFloat : ErrorMessage -> Validator String Float
isFloat err =
    String.toFloat >> Result.fromMaybe err >> Result.mapError (always err)


{-| Return an `Err errorMessage` if the given value isn't positive float number,
otherwise return `Ok value`. It contains also float validation of value.
-}
isPositiveFloat : ErrorMessage -> Validator String Float
isPositiveFloat err =
    let
        isPositive e fl =
            if fl > 0 then
                Ok fl

            else
                Err e
    in
    composite (isFloat err) (isPositive err)


{-| Return an `Err errorMessage` if the given value isn't int number, otherwise
return `Ok value`.

    import Validation exposing (Validator, ErrorMessage)

    intValidation : Validator String Int
    intValidation =
        isInt "It is not int number!"

    intValidation "108" -- Ok 108
    intValidation "3.14" -- Err "It is not int number!"

-}
isInt : ErrorMessage -> Validator String Int
isInt err =
    String.toInt >> Result.fromMaybe err >> Result.mapError (always err)


{-| Return an `Err errorMessage` if the given value isn't positive int number,
otherwise return `Ok value`. It contains also int validation of value.
-}
isPositiveInt : ErrorMessage -> Validator String Int
isPositiveInt err =
    let
        isPositive e i =
            if i > 0 then
                Ok i

            else
                Err e
    in
    composite (isInt err) (isPositive err)


{-| Return an `Err errorMessage` if the given boolean value is false, otherwise
return `Ok True`.
-}
isTrue : ErrorMessage -> Validator Bool Bool
isTrue err b =
    if b then
        Ok b

    else
        Err err


{-| Validate Field

Return an `Err errorMessage` if the given value of `Field Valid a` isn't same as
validation argument, otherwise return `Ok validation argument` for others `Validity`
or for `Valid a` is `Ok value`.

    import Validation exposing (Validator, ErrorMessage, Field)

    pass : Field String String
    pass =
        Field "" (Valid "password*")

    confirmPasswordValidation : Validator String String
    confirmPasswordValidation =
        isEqualTo pass "The passwords don't match."

    confirmPasswordValidation "password*" -- Ok password*
    confirmPasswordValidation "pasword*"  -- Err "The passwords don't match."

-}
isEqualTo : Field raw a -> ErrorMessage -> Validator a a
isEqualTo otherField err a2 =
    case validity otherField of
        Valid a1 ->
            if a1 == a2 then
                Ok a2

            else
                Err err

        _ ->
            Ok a2


{-| Return an `Err errorMessage` if the given string is empty, otherwise
return `Ok value`.
-}
isNotEmpty : ErrorMessage -> Validator String String
isNotEmpty err value =
    if String.isEmpty value then
        Err err

    else
        Ok value


{-| Return an `Err errorMessage` if the given string isn't correct an email address,
otherwise return `Ok value`.
-}
isEmail : ErrorMessage -> Validator String String
isEmail err value =
    if Regex.contains validEmailPattern value then
        Ok value

    else
        Err err


{-| Return an `Err errorMessage` if the given string isn't correct an url path,
otherwise return `Ok value`.
-}
isUrl : ErrorMessage -> Validator String String
isUrl err value =
    if Regex.contains validUrlPattern value then
        Ok value

    else
        Err err


{-| Return an `Err errorMessage` if the given value isn't in list, otherwise
return `Ok value`.

    import Validation exposing (Validator, ErrorMessage)

    elInListValidation : Validator (Int, List Int) Int
    elInListValidation =
        isInList "Given value is not in list!"

    elInListValidation (3, [1, 2, 3]) -- Ok 3
    elInListValidation (5, [1, 2, 3]) -- Err "Given value is not in list!"

-}
isInList : ErrorMessage -> Validator ( a, List a ) a
isInList err tpl =
    let
        ( el, list_ ) =
            tpl
    in
    if List.member el list_ then
        Ok el

    else
        Err err


{-| Return false if `Field` hasn't validity `Valid a`, otherwise
return true.

    import Validation exposing (Field, field, preValidatedField)

    intNotValidValue = field "50"        -- Field "50" NotValidated
    intValidValue = preValidatedField 50 -- Field "50" (Valid 50)

    isValidField intNotValidValue -- False
    isValidField intValidValue    -- True

-}
isValidField : Field raw a -> Bool
isValidField isValidF =
    case validity isValidF of
        Valid _ ->
            True

        _ ->
            False



-- Internal
-- copied from elm-validate


validEmailPattern : Regex.Regex
validEmailPattern =
    "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never



-- copied from etaque/elm-simple-form


validUrlPattern : Regex.Regex
validUrlPattern =
    "^(https?://)?([\\da-z\\.-]+)\\.([a-z\\.]{2,6})([\\w \\.-]*)*/?$"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never
