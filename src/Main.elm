module Main exposing (..)

import Create
import Html exposing (..)
import Login
import Navigation as Nav
import Signup
import Timeline


-- MODEL


type Model
    = LoginPage Login.Model
    | SignupPage Signup.Model
    | TimelinePage Timeline.Model
    | CreatePage Create.Model



-- UPDATE


type Msg
    = LoginMsg Login.Msg
    | SignupMsg Signup.Msg
    | TimelineMsg Timeline.Msg
    | CreateMsg Create.Msg
    | NewPage Nav.Location


bimap : (a -> c) -> (b -> d) -> ( a, b ) -> ( c, d )
bimap f g ( alpha, beta ) =
    ( f alpha, g beta )


doNothing : a -> ( a, Cmd msg )
doNothing x =
    ( x, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case ( msg, model ) of
        ( LoginMsg msg, LoginPage model ) ->
            bimap LoginPage identity (Login.update msg model)

        ( SignupMsg msg, SignupPage model ) ->
            bimap SignupPage identity (Signup.update msg model)

        ( TimelineMsg msg, TimelinePage model ) ->
            bimap TimelinePage identity (Timeline.update msg model)

        ( CreateMsg msg, CreatePage model ) ->
            bimap CreatePage identity (Create.update msg model)

        _ ->
            -- In this case, a page has recieved a message it cannot deal with,
            -- so we do nothing. We should probably throw an error somehow.
            model |> doNothing



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        LoginPage model ->
            map LoginMsg (Login.view model)

        SignupPage model ->
            map SignupMsg (Signup.view model)

        TimelinePage model ->
            map TimelineMsg (Timeline.view model)

        CreatePage model ->
            map CreateMsg (Create.view model)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []



-- MAIN


init : Nav.Location -> ( Model, Cmd msg )
init location =
    let
        ( model, cmd ) =
            Login.init
    in
    ( LoginPage model, cmd )


main : Program Never Model Msg
main =
    Nav.program NewPage
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
