module Validators
    exposing
        ( isFloat
        , isPositiveFloat
        , isInt
        , isPositiveInt
        , isNatural
        , isTrue
        , isEqualTo
        , isNotEmpty
        , isEmail
        , isUrl
        , isInList
        )

import Regex exposing (Regex)
import Validation exposing (ErrorMessage, Validator)


isFloat : ErrorMessage -> Validator String Float
isFloat err =
    String.toFloat >> (Result.mapError (always err))


isPositiveFloat : ErrorMessage -> Validator Float Float
isPositiveFloat err fl =
    if fl >= 0 then
        Ok fl
    else
        Err err


isInt : ErrorMessage -> Validator String Int
isInt err =
    String.toInt >> (Result.mapError (always err))


isPositiveInt : ErrorMessage -> Validator Int Int
isPositiveInt err i =
    if i >= 0 then
        Ok i
    else
        Err err


isNatural : ErrorMessage -> Validator String Int
isNatural err =
    isInt err >&& isPositiveInt err


isTrue : ErrorMessage -> Validator Bool Bool
isTrue err b =
    if b then
        Ok b
    else
        Err err


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


isNotEmpty : ErrorMessage -> Validator String String
isNotEmpty err value =
    if String.isEmpty value then
        Err err
    else
        Ok value


isEmail : ErrorMessage -> Validator String String
isEmail err value =
    if Regex.contains validEmailPattern value then
        Ok value
    else
        Err err


isUrl : ErrorMessage -> Validator String String
isUrl err value =
    if Regex.contains validUrlPattern value then
        Ok value
    else
        Err err



-- check if the string is included in the given list


isInList : a -> List a -> ErrorMessage -> Validator String a
isInList s list_ err _ =
    if List.member s list_ then
        Ok s
    else
        Err err



-- Internal
-- stolen from elm-validate


validEmailPattern : Regex
validEmailPattern =
    Regex.regex "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> Regex.caseInsensitive



-- stolen from etaque/elm-simple-form


validUrlPattern : Regex
validUrlPattern =
    Regex.regex "^(https?://)?([\\da-z\\.-]+)\\.([a-z\\.]{2,6})([\\w \\.-]*)*/?$"
        |> Regex.caseInsensitive
