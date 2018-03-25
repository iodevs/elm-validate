module Transformation
    exposing
        ( field
        , toModel
        )

{-| This module help transform validated forms to models.


# Helpers

@docs field, toModel

-}

import Validation
    exposing
        ( Field
        , Validity(..)
        , validity
        )


{-| -}
type Transformer data form
    = Transformer (Result String data) form


type alias Creator a b =
    a -> Result String b


type alias Accessor form raw a =
    form -> Field raw a


{-| -}
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


{-| -}
toModel : model -> (Transformer model form -> Transformer data form) -> form -> Result String data
toModel model f form =
    let
        (Transformer result _) =
            f (Transformer (Ok model) form)
    in
        result
