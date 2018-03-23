module Transformation exposing (field, toModel)

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


type alias Container data form =
    ( Result String data, form )


{-| -}
field : (form -> Field raw a) -> Container (a -> b) form -> Container b form
field acs ( model, form ) =
    case model of
        Ok mdl ->
            case validity (acs form) of
                NotValidated ->
                    ( Err "Form is invalid!!!", form )

                Invalid _ ->
                    ( Err "Form is invalid!!!", form )

                Valid val ->
                    ( Ok (mdl val), form )

        Err msg ->
            ( Err msg, form )


{-| -}
toModel : model -> (Container model form -> Container data form) -> form -> Result String data
toModel model f form =
    case f ( Ok model, form ) of
        ( Ok model, _ ) ->
            Ok model

        ( Err msg, _ ) ->
            Err msg
