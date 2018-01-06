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
    , id : Int
    , pendingTaskTitle : String
    }

type alias Board =
    { title : String
    , schedulers : List Scheduler
    , id : Int
    , pendingSchedulerTitle : String
    , activeSchedulerIndex : Int
    }

type alias Dashboard =
    { currentBoardTitle : String
    , boards : List Board
    , currentSchedulerTitle : String
    , activeBoardIndex : Int
    }



init : (Model, Cmd Msg)
init =
    ( initialModel, Cmd.none )

initialModel : Model
initialModel =
    Model (Dashboard "" [] "" 0) 0 False 0


-- MODEL

type alias Model =
    { dashboard : Dashboard
     , currentIndex : Int
     , forwardToDetails : Bool
     , selectedBoardIndex : Int
     }


-- UPDATE

type Msg =
    ChangeCurrentBoardIitle String
    | AddBoard
    | SwitchToBoard Int
    | ChangeCurrentSchedulerTitle String
    | AddScheduler
    | ActionBack
    | ChangeCurrentTaskTitle String
    | AddTask
    | ChangeCurrentSchedulerIndex Int


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
                newDashboard = { oldDashboard | boards = ( Board currentBoardTitle [] model.currentIndex "" 0 ) :: oldDashboard.boards
                    , currentBoardTitle = "" }
            in
                ( { model | dashboard = newDashboard
                    , currentIndex = model.currentIndex + 1
                  }, Cmd.none )
        SwitchToBoard id ->
            ( { model | selectedBoardIndex = id, forwardToDetails = True }, Cmd.none )
        ChangeCurrentSchedulerTitle title ->
            let
                index = model.selectedBoardIndex
                oldBoardA = List.head (List.filter (\ board -> board.id == index ) model.dashboard.boards)
                oldBoard = checkBoard oldBoardA
                updatedBoard = { oldBoard | pendingSchedulerTitle = title }
                filteredBoards = List.filter (\ board -> board.id /= index ) model.dashboard.boards
                newBoards = updatedBoard :: filteredBoards
                oldDashboard = model.dashboard
                newDashboard = { oldDashboard | boards = newBoards }
            in
                ( { model | dashboard = newDashboard }, Cmd.none )
        AddScheduler ->
            let
                selectedBoardIndex = model.selectedBoardIndex
                oldBoardA = List.head (List.filter ( \ board -> board.id == selectedBoardIndex ) model.dashboard.boards)
                oldBoard = checkBoard oldBoardA
                title = oldBoard.pendingSchedulerTitle
                newBoard = { oldBoard | schedulers = ( Scheduler title "" [] model.currentIndex "") :: oldBoard.schedulers
                    , pendingSchedulerTitle = "" }
                filteredBoards = List.filter ( \ board -> board.id /= selectedBoardIndex ) model.dashboard.boards
                updatedBoards = newBoard :: filteredBoards
                oldDashboard = model.dashboard
                newDashboard = { oldDashboard | boards = updatedBoards }
            in
                ( { model | dashboard = newDashboard
                    , currentIndex = model.currentIndex + 1 }, Cmd.none )
        ActionBack ->
            ( { model | forwardToDetails = False }, Cmd.none )
        ChangeCurrentTaskTitle title ->
            let
                boardA = List.head (List.filter (\ board -> board.id == model.selectedBoardIndex) model.dashboard.boards)
                oldBoard = checkBoard boardA
                schedulerA = List.head (List.filter (\ scheduler -> scheduler.id == oldBoard.activeSchedulerIndex) oldBoard.schedulers)
                oldScheduler = checkScheduler schedulerA
                newScheduler = { oldScheduler | pendingTaskTitle = title }
                filteredSchedulers = List.filter (\ scheduler -> scheduler.id /= oldBoard.activeSchedulerIndex) oldBoard.schedulers
                updatedBoard = { oldBoard | schedulers = newScheduler :: filteredSchedulers }
                oldDashboard = model.dashboard
                filteredBoards = List.filter (\ board -> board.id /= model.selectedBoardIndex) model.dashboard.boards
                newDashboard = { oldDashboard | boards = updatedBoard :: filteredBoards }
            in
                ( { model | dashboard = newDashboard }, Cmd.none )
        AddTask ->
            let
                boardA = List.head (List.filter (\ board -> board.id == model.selectedBoardIndex) model.dashboard.boards)
                oldBoard = checkBoard boardA
                schedulerA = List.head (List.filter (\ scheduler -> scheduler.id == oldBoard.activeSchedulerIndex) oldBoard.schedulers)
                oldScheduler = checkScheduler schedulerA
                newScheduler = { oldScheduler |
                    tasks = ( Task oldScheduler.pendingTaskTitle False model.currentIndex ) :: oldScheduler.tasks
                    , pendingTaskTitle = ""
                    }
                filteredSchedulerList = List.filter (\ scheduler -> scheduler.id /= oldBoard.activeSchedulerIndex) oldBoard.schedulers
                updatedSchedulers = newScheduler :: filteredSchedulerList
                newBoard = { oldBoard | schedulers = updatedSchedulers }
                filteredBoardList = List.filter (\ board -> board.id == model.selectedBoardIndex) model.dashboard.boards
                updatedBoards = newBoard :: filteredBoardList
                oldDashboard = model.dashboard
                newDashboard = { oldDashboard | boards = updatedBoards }
            in
                ( { model | dashboard = newDashboard
                    , currentIndex = model.currentIndex + 1 }, Cmd.none )
        ChangeCurrentSchedulerIndex id ->
            let
                boardA = List.head (List.filter (\ board -> board.id == model.selectedBoardIndex) model.dashboard.boards)
                oldBoard = checkBoard boardA
                newBoard = { oldBoard | activeSchedulerIndex = id }
                boardList = List.filter (\ board -> board.id /= model.selectedBoardIndex) model.dashboard.boards
                updatedBoardList = newBoard :: boardList
                oldDashboard = model.dashboard
                newDashboard = { oldDashboard | boards = updatedBoardList }
            in
                ( { model | dashboard = newDashboard }, Cmd.none )



checkScheduler : Maybe Scheduler -> Scheduler
checkScheduler a =
    case a of
        Nothing ->
            Scheduler "" "" [] 0 ""
        Just a ->
            Scheduler a.title a.currentTask a.tasks a.id a.pendingTaskTitle


checkBoard : Maybe Board -> Board
checkBoard a =
    case a of
        Nothing ->
            Board "" [] 0 "" 0
        Just a ->
            Board a.title a.schedulers a.id a.pendingSchedulerTitle a.activeSchedulerIndex


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ (if model.forwardToDetails then detailsView model
            else indexView model)
        ]

detailsView : Model -> Html Msg
detailsView model =
    let
        boardList = List.filter (\ board -> board.id == model.selectedBoardIndex ) model.dashboard.boards
        board = List.head boardList
    in
        case board of
            Nothing ->
                div [] [ text "No board found!" ]
            Just a ->
                boardDetailsView (Board a.title a.schedulers a.id a.pendingSchedulerTitle a.activeSchedulerIndex)



boardDetailsView : Board -> Html Msg
boardDetailsView board =
    div []
        [ h1[] [ text board.title ]
        , button [ onClick ActionBack, style [ ("float", "right") ] ] [ text "Back "]
        , br [] []
        , input [ placeholder "Enter scheduler title"
            , value board.pendingSchedulerTitle
            , onInput ChangeCurrentSchedulerTitle ] []
        , button [ onClick AddScheduler ] [ text "Add Scheduler" ]
        , br [] []
        , ul [] ( List.map schedulerView board.schedulers )
        ]


schedulerView : Scheduler -> Html Msg
schedulerView scheduler =
    li []
        [ h4 [] [ text scheduler.title ]
        , input [ onClick (ChangeCurrentSchedulerIndex scheduler.id)
        , onInput ChangeCurrentTaskTitle, placeholder "Title for new task!"
        , value scheduler.pendingTaskTitle ] []
        , button [ onClick AddTask ] [ text "Add Task" ]
        , ul [] ( List.map taskView scheduler.tasks )
        ]


taskView : Task -> Html Msg
taskView task =
    li []
        [ text task.title
        , input [ type_ "checkbox", checked task.completed ] []
        ]



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
        [ h3 [ onClick (SwitchToBoard board.id) ] [ text board.title ]
        ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none