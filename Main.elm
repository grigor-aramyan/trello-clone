module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


init : (Model, Cmd Msg)
init =
    ( initialModel, Cmd.none )

initialModel : Model
initialModel =
    Model ""


-- MODEL

type alias Model =
    { something : String }


-- UPDATE

type Msg =
    DumbMsg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        DumbMsg ->
            (model, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
    div []
    [ text "hello, it's trello clone"
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none