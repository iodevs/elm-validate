module Lib.Validation exposing (..)

import Regex exposing(Regex)



type Validity a
    = NotValidated
    | Valid a
    | Invalid String


type Event raw
    = OnSubmit
    | OnBlur
    | OnRelatedChange
    | OnChange raw


type Field raw a =
    Field raw (Validity a)


type alias OptionalField raw a =
    Field raw (Maybe a)


type alias Validator a b =
    a -> Result String b


type alias ErrorMessage =
    String


rawValue : Field b a -> b
rawValue (Field rawValue _) = rawValue


validity : Field raw a -> Validity a
validity (Field _ validity) = validity


field : b -> Field b a
field value =
    Field value NotValidated


setError : String -> Field b a -> Field b a1
setError err (Field value _) =
    Field value (Invalid err)


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
                validate value |> toValidity)


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


(|:) : Validity (a -> b) -> Validity a -> Validity b
(|:) = flip applyValidity


(>&&) : Validator a b -> Validator b c -> Validator a c
(>&&) f g = f >> Result.andThen g


-- Usage below functions

extractError : Field raw a -> Maybe String
extractError field =
    case validity field of
        Invalid err ->
            Just err

        _ ->
            Nothing


optional : Validator String a -> Validator String (Maybe a)
optional validate s =
    if String.isEmpty s then
        Ok Nothing
    else
        validate s |> Result.map Just


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
