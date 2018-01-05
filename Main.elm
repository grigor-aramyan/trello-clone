module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main : Program Never Model Msg
main =
    Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


-- MODEL

type alias Task =
    { title : String
    , completed : Bool
    , id : Int
    }

type alias Scheduler =
    { title : String
    , currentTask : String
    , tasks : List Task
    }

type alias Board =
    { title : String
    , schedulers : List Scheduler
    , id : Int
    }

type alias Dashboard =
    { currentBoardTitle : String
    , boards : List Board
    }



init : (Model, Cmd Msg)
init =
    ( initialModel, Cmd.none )

initialModel : Model
initialModel =
    Model (Dashboard "" []) 0 False


-- MODEL

type alias Model =
    { dashboard : Dashboard
     , currentIndex : Int
     , forwardToDetails : Bool
     }


-- UPDATE

type Msg =
    ChangeCurrentBoardIitle String
    | AddBoard


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeCurrentBoardIitle title ->
            let
                oldDashboard = model.dashboard
                newDashboard = { oldDashboard | currentBoardTitle = title }
            in
                ( { model | dashboard = newDashboard }, Cmd.none)
        AddBoard ->
            let
                oldDashboard = model.dashboard
                currentBoardTitle = oldDashboard.currentBoardTitle
                newDashboard = { oldDashboard | boards = ( Board currentBoardTitle [] model.currentIndex ) :: oldDashboard.boards
                    , currentBoardTitle = "" }
            in
                ( { model | dashboard = newDashboard
                    , currentIndex = model.currentIndex + 1
                  }, Cmd.none )


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ (if model.forwardToDetails then detailsView model
            else indexView model)
        ]

detailsView : Model -> Html Msg
detailsView model =
    text "details view here"


indexView : Model -> Html Msg
indexView model =
    div []
        [ input [ value model.dashboard.currentBoardTitle
            , onInput ChangeCurrentBoardIitle
            , placeholder "Enter board title"
            ] []
        , button [ onClick AddBoard ] [ text "Add board" ]
        , br [] []
        , ul [] ( List.map boardView model.dashboard.boards )
        ]


boardView : Board -> Html Msg
boardView board =
    li []
        [ h3 [] [ text board.title ]
        ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none