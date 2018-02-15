module Main exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onBlur, onClick, onInput, onSubmit)

import Http
import Json.Decode as Decode
import Json.Encode as Encode

import Validation exposing(..)
import Validators exposing(..)


main : Program Never Model Msg
main =
    program
        { init = ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }


type alias Model =
    { email : Field String String
    , password : Field String String
    , confirmPassword : Field String String
    , acceptPolicy : Field Bool Bool
    , status : SubmissionStatus
    }


initModel : Model
initModel =
    { email = field ""
    , password = field ""
    , confirmPassword = field ""
    , acceptPolicy = field False
    , status = NotSubmitted
    }


type SubmissionStatus
    = NotSubmitted
    | InProcess
    | Succeeded
    | Failed


type Msg
    = InputEmail String
    | BlurEmail
    | InputPassword String
    | BlurPassword
    | InputConfirmPassword String
    | BlurConfirmPassword
    | CheckAcceptPolicy Bool
    | Submit
    | SubmitResponse (Result Http.Error ())



-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        InputEmail e ->
            ({model
                | email = model.email
                    |> validate (OnChange e) emailValidation
             }
            , Cmd.none
            )

        BlurEmail ->
            ({model
                | email = model.email
                    |> validate OnBlur emailValidation
             }
            , Cmd.none
            )

        InputPassword p ->
            let
                password =
                    model.password
                        |> validate (OnChange p) passwordValidation
            in

            ({model
                | password = password
                , confirmPassword = model.confirmPassword
                    |> validate OnRelatedChange (confirmPasswordValidation password)
             }
            , Cmd.none
            )

        BlurPassword ->
            ({model
                | password = model.password
                    |> validate OnBlur passwordValidation
             }
            , Cmd.none
            )

        InputConfirmPassword p ->
            ({model
                | confirmPassword = model.confirmPassword
                    |> validate (OnChange p) (confirmPasswordValidation model.password)
             }
            , Cmd.none
            )

        BlurConfirmPassword ->
            ({model
                | confirmPassword = model.confirmPassword
                    |> validate OnBlur (confirmPasswordValidation model.password)
             }
            , Cmd.none
            )

        CheckAcceptPolicy a ->
            ({model
                | acceptPolicy = field a
             }
            , Cmd.none
            )

        Submit ->
            model |> validateModel |> submitIfValid

        SubmitResponse (Ok ()) ->
            ({initModel | status = Succeeded
             }
            , Cmd.none
            )

        SubmitResponse (Err _) ->
            ({model
                | status = Failed
             }
            , Cmd.none
            )



-- View

view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ Html.form
            [ class "form"
            , onSubmit Submit
            , novalidate True
            ]
            [ header model
            , body model
            , footer model
            ]
        , div [ class "model-form" ]
            (modelForm model)
        ]


header : Model -> Html Msg
header model =
    div [ class "header" ]
        [ h1 [] [ text "Register" ]
        , renderStatus model.status
        ]


renderStatus : SubmissionStatus -> Html Msg
renderStatus status =
    case status of
        NotSubmitted ->
            div [] []

        InProcess ->
            div [] [ text "Your request is being sent." ]

        Succeeded ->
            div [] [ text "Your request has been recieved." ]

        Failed ->
            div [ class "msg--error" ]
                [ text "There was an error, please try again." ]


body : Model -> Html Msg
body model =
    div [ class "content" ]
        [ div []
            [ input
                [ placeholder "Your email *"
                , type_ "email"
                , required True
                , value (model.email |> rawValue)
                , onInput InputEmail
                , onBlur BlurEmail
                ]
                []
            , errorLabel model.email
            ]
        , div []
            [ input
                [ placeholder "Your password *"
                , type_ "password"
                , required True
                , value (model.password |> rawValue)
                , onInput InputPassword
                , onBlur BlurPassword
                ]
                []
            , errorLabel model.password
            ]
        , div []
            [ input
                [ placeholder "Your confirm password *"
                , type_ "password"
                , required True
                , value (model.confirmPassword |> rawValue)
                , onInput InputConfirmPassword
                , onBlur BlurConfirmPassword
                ]
                []
            , errorLabel model.confirmPassword
            ]
        , div []
            [ input
                [ type_ "checkbox"
                , id "terms"
                , value (model.acceptPolicy |> rawValue |> toString )
                , onCheck CheckAcceptPolicy
                ]
                []
            , label [ for "terms" ]
                [ text "I accept the privacy policy" ]
            ]
        , div []
            [ errorLabel model.acceptPolicy ]
        ]


errorLabel : Field raw a -> Html Msg
errorLabel field =
    div [ class "msg msg--error" ]
        [ field
            |> extractError
            |> Maybe.withDefault ""
            |> text
        ]


footer : Model -> Html Msg
footer model =
    div []
        [ button
            [ class "btn__submit"
            , type_ "submit"
            , disabled (model.status == InProcess)
            ]
            [ text "Submit" ]
        ]


modelForm : Model-> List (Html msg)
modelForm model =
    [ div [ class "header" ]
            [ h1 [] [ text "Model state" ] ]
    , div []
        [ text "{ email ="
        , p [ class "model__el" ]
            [ model.email |> toString |> text ]
        ]
    , div []
        [ text ", password  ="
        , p [ class "model__el" ]
            [ model.password  |> toString |> text ]
        ]
    , div []
        [ text ", confirmPassword  ="
        , p [ class "model__el" ]
            [ model.confirmPassword  |> toString |> text ]
        ]
    , div []
        [ text ", acceptPolicy  ="
        , p [ class "model__el" ]
            [ model.acceptPolicy  |> toString |> text ]
        ]
    , div []
        [ text ", status  ="
        , div [ class "model__el" ]
            [ model.status |> toString |> text ]
        , text "}"
        ]
    ]



-- Validation

emailValidation : Validator String String
emailValidation =
    isNotEmpty "An email is required."
        >&& isEmail "Please ensure this is a valid email."


passwordValidation : Validator String String
passwordValidation =
    isNotEmpty "Please enter a password."


confirmPasswordValidation : Field raw String -> Validator String String
confirmPasswordValidation password =
    isNotEmpty "Please enter a password."
        >&& isEqualTo password "The passwords don't match."


validateModel : Model -> Model
validateModel model =
    let
        email =
            model.email |> validate OnSubmit emailValidation

        password =
            model.password |> validate OnSubmit passwordValidation

        confirmPassword =
            model.confirmPassword
                |> validate OnSubmit (confirmPasswordValidation password)

        acceptPolicy =
            model.acceptPolicy
                |> validate OnSubmit (isTrue "You must accept the policy.")
    in
        {model
            | email = email
            , password = password
            , confirmPassword = confirmPassword
            , acceptPolicy = acceptPolicy
        }

submitIfValid : Model -> (Model, Cmd Msg)
submitIfValid model =
    let
        submissionResult =
            Valid submit
                |: (validity model.email)
                |: (validity model.password)
                |: (validity model.confirmPassword)
                |: (validity model.acceptPolicy)
    in
        case submissionResult of
            Valid cmd ->
                ({model | status = InProcess}
                , cmd
                )

            _ ->
                (model, Cmd.none)



-- Send request

submit : String -> String -> String -> Bool -> Cmd Msg
submit email password _ _ =
    let
        url = "http://localhost:8080/api/register"

        json =
            Encode.object
                [ ("email", Encode.string email)
                , ("password", Encode.string password)
                ]

        decoder =
            Decode.string |> Decode.map (always ())

        request =
            Http.post url (Http.jsonBody json) decoder
    in
        request
            |> Http.send SubmitResponse

