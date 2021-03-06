module Validators exposing
    ( isFloat, isInt, isAtLeast, isAtMost, isGreaterThan, isLessThan, isRange
    , isNotEmpty, isEmail, isUrl
    , isInList
    , isTrue, isEqualTo, isValidField
    )

{-| This module provides a few functions for validating data.


# Numbers validation

@docs isFloat, isInt, isAtLeast, isAtMost, isGreaterThan, isLessThan, isRange


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


{-| Returns an `Err errorMessage` if the given value isn't float number, otherwise
return `Ok value`.

    import Validation exposing (Validator, ErrorMessage)

    floatValidation : Validator String Float
    floatValidation =
        isFloat "The value is not float number!"

    floatValidation "5.39161" -- Ok 5.39161
    floatValidation "t.39161" -- Err "It is not float number!"

-}
isFloat : ErrorMessage -> Validator String Float
isFloat err =
    String.toFloat >> Result.fromMaybe err >> Result.mapError (always err)


{-| Returns an `Err errorMessage` if the given value isn't int number, otherwise
return `Ok value`.

    import Validation exposing (Validator, ErrorMessage)

    intValidation : Validator String Int
    intValidation =
        isInt "The value is not int number!"

    intValidation "108"  -- Ok 108
    intValidation "3.14" -- Err "It is not int number!"

-}
isInt : ErrorMessage -> Validator String Int
isInt err =
    String.toInt >> Result.fromMaybe err >> Result.mapError (always err)


{-| Returns an `Err errorMessage2` or `Err errorMessage1` if the given value isn't
less than defined value (defined by user) or if first validation failed.
Otherwise return `Ok value`.

    import Validation exposing (ErrorMessage, Validator)
    import Validators exposing (isLessThan)

    isLessThanInt : Validator String Int
    isLessThanInt =
        isLessThan (isInt "The value is not int number!") 5 "The value is not less than 5!"

    isLessThanInt "3"   -- Ok 3
    isLessThanInt "6"   -- Err "The value is not less than 5!"
    isLessThanInt "6.3" -- Err "The value is not int number!"

-}
isLessThan : Validator String comparable -> comparable -> ErrorMessage -> Validator String comparable
isLessThan validator_ val err =
    let
        isLessThan_ e num =
            if num < val then
                Ok num

            else
                Err e
    in
    composite validator_ (isLessThan_ err)


{-| Returns an `Err errorMessage2` or `Err errorMessage1` if the given value isn't
less or equal to defined value (defined by user) or if first validation failed.
Otherwise return `Ok value`.
-}
isAtMost : Validator String comparable -> comparable -> ErrorMessage -> Validator String comparable
isAtMost validator_ val err =
    let
        isLessThan_ e num =
            if num <= val then
                Ok num

            else
                Err e
    in
    composite validator_ (isLessThan_ err)


{-| Returns an `Err errorMessage2` or `Err errorMessage1` if the given value isn't
greater than defined value (defined by user) or if first validation failed.
Otherwise return `Ok value`.
-}
isGreaterThan : Validator String comparable -> comparable -> ErrorMessage -> Validator String comparable
isGreaterThan validator_ val err =
    let
        isGreaterThan_ e num =
            if val < num then
                Ok num

            else
                Err e
    in
    composite validator_ (isGreaterThan_ err)


{-| Returns an `Err errorMessage2` or `Err errorMessage1` if the given value isn't
greater or equal to defined value (defined by user) or if first validation failed.
Otherwise return `Ok value`.
-}
isAtLeast : Validator String comparable -> comparable -> ErrorMessage -> Validator String comparable
isAtLeast validator_ val err =
    let
        isAtLeast_ e num =
            if val <= num then
                Ok num

            else
                Err e
    in
    composite validator_ (isAtLeast_ err)


{-| Returns an `Err errorMessage2` or `Err errorMessage1` if the given value isn't
in range (mathematically speaking it's closed interval) or if first validation failed.
Otherwise return `Ok value`. First number has to be less than second.

    import Validation exposing (ErrorMessage, Validator)
    import Validators exposing (isFloat, isRange)

    floatRange : Validator String Float
    floatRange =
        isRange (isFloat "The value is not float number!") 0 5.5 "The value is not in range!"

    floatRange "3.8" -- Ok 3.8
    floatRange "6.1" -- Err "The value is not in range!"
    floatRange "6.x" -- Err "The value is not float number!"

-}
isRange : Validator String comparable -> comparable -> comparable -> ErrorMessage -> Validator String comparable
isRange validator_ lowNum hightNum err =
    let
        isRange_ e num =
            if lowNum <= num && num <= hightNum then
                Ok num

            else
                Err e
    in
    composite validator_ (isRange_ err)


{-| Returns an `Err errorMessage` if the given boolean value is false, otherwise
return `Ok True`.
-}
isTrue : ErrorMessage -> Validator Bool Bool
isTrue err b =
    if b then
        Ok b

    else
        Err err


{-| Validate Field

Returns an `Err errorMessage` if the given value of `Field Valid a` isn't same as
validation argument, otherwise return `Ok validation argument` for others `Validity`
or for `Valid a` is `Ok value`.

    import Validation exposing (Validator, ErrorMessage, Field)

    pass : Field String String
    pass =
        Field "" (Valid "password*")

    confirmPasswordValidation : Validator String String
    confirmPasswordValidation =
        isEqualTo pass "The passwords don't match."

    confirmPasswordValidation "password*" -- Ok "password*"
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


{-| Returns an `Err errorMessage` if the given string is empty, otherwise
return `Ok value`.
-}
isNotEmpty : ErrorMessage -> Validator String String
isNotEmpty err value =
    if String.isEmpty value then
        Err err

    else
        Ok value


{-| Returns an `Err errorMessage` if the given string isn't correct an email address,
otherwise return `Ok value`.
-}
isEmail : ErrorMessage -> Validator String String
isEmail err value =
    if Regex.contains validEmailPattern value then
        Ok value

    else
        Err err


{-| Returns an `Err errorMessage` if the given string isn't correct an url path,
otherwise return `Ok value`.
-}
isUrl : ErrorMessage -> Validator String String
isUrl err value =
    if Regex.contains validUrlPattern value then
        Ok value

    else
        Err err


{-| Returns an `Err errorMessage` if the given value isn't in list, otherwise
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


{-| Returns false if `Field` hasn't validity `Valid a`, otherwise
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
-- copied from rtfeldman/elm-validate


validEmailPattern : Regex.Regex
validEmailPattern =
    "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never



-- inspired by
-- https://gist.github.com/dperini/729294
-- https://mathiasbynens.be/demo/url-regex
-- copied from
-- https://github.com/MarQuisKnox/regex-weburl.js/blob/master/regex-weburl.js


validUrlPattern : Regex.Regex
validUrlPattern =
    --"^(?:(?:https?|ftp)://)(?:\\S+(?::\\S*)?@)?(?:(?!10(?:\\.\\d{1,3}){3})(?!127(?:\\.\\d{1,3}){3})(?!169\\.254(?:\\.\\d{1,3}){2})(?!192\\.168(?:\\.\\d{1,3}){2})(?!172\\.(?:1[6-9]|2\\d|3[0-1])(?:\\.\\d{1,3}){2})(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}(?:\\.(?:[1-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))|(?:(?:[a-z\\u00a1-\\uffff0-9]+-?)*[a-z\\u00a1-\\uffff0-9]+)(?:\\.(?:[a-z\\u00a1-\\uffff0-9]+-?)*[a-z\\u00a1-\\uffff0-9]+)*(?:\\.(?:[a-z\\u00a1-\\uffff]{2,})))(?::\\d{2,5})?(?:/[^\\s]*)?$"
    "^(?:(?:https?|ftp)://)(?:\\S+(?::\\S*)?@)?(?:(?!(?:10|127)(?:\\.\\d{1,3}){3})(?!(?:169\\.254|192\\.168)(?:\\.\\d{1,3}){2})(?!172\\.(?:1[6-9]|2\\d|3[0-1])(?:\\.\\d{1,3}){2})(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}(?:\\.(?:[1-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))|(?:(?:[a-z\\u00a1-\\uffff0-9]-*)*[a-z\\u00a1-\\uffff0-9]+)(?:\\.(?:[a-z\\u00a1-\\uffff0-9]-*)*[a-z\\u00a1-\\uffff0-9]+)*(?:\\.(?:[a-z\\u00a1-\\uffff]{2,})))(?::\\d{2,5})?(?:\\/\\S*)?$"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never
