module Timeline exposing (..)

import Commands
import Html exposing (..)


-- MODEL


type Model
    = Model



-- UPDATE


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    Commands.doNothing model



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ text "Timeline!" ]
