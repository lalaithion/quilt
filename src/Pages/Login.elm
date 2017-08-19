module Login exposing (..)

import Commands
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


-- MODEL


type alias Model =
    { email : String, password : String, error : Maybe String }



-- UPDATE


type Msg
    = Login
    | Email String
    | Password String
    | Error String


login : Model -> ( Model, Cmd msg )
login model =
    if model.email == model.password then
        Commands.doNothing model
    else
        update (Error "woe, woe!") model


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Login ->
            login model

        Email str ->
            Commands.doNothing { model | email = str }

        Password str ->
            Commands.doNothing { model | password = str }

        Error str ->
            Commands.doNothing { model | error = Just str }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "form", class "login" ]
            [ input [ type_ "text", value model.email, placeholder "Enter Your Email", onInput Email ] []
            , input [ type_ "text", value model.password, placeholder "Enter Your Password", onInput Password ] []
            , button [ onClick Login ] [ text "login" ]
            ]
        , div [ class "error" ] [ text (Maybe.withDefault "" model.error) ]
        ]



-- INIT


init : ( Model, Cmd msg )
init =
    ( Model "" "" Nothing, Cmd.none )
