module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onMouseDown)
import Html5.DragDrop as DragDrop
import Style exposing (..)

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
    , stable_id : Int
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
    Model (Dashboard "" [] "" 0) 0 False 0 DragDrop.init -1


-- MODEL

type alias Model =
    { dashboard : Dashboard
     , currentIndex : Int
     , forwardToDetails : Bool
     , selectedBoardIndex : Int
     , dragDrop : DragDrop.Model Int Int
     , clickedSchedulerIndex : Int
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
    | DragDropMsg (DragDrop.Msg Int Int)
    | SetClickedSchedulerIndex Int
    | CheckTask Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CheckTask taskId ->
            let
                activeBoardIndex = model.selectedBoardIndex
                oldBoardA = List.head (List.filter (\ board -> board.id == activeBoardIndex ) model.dashboard.boards )
                oldBoard = checkBoard oldBoardA
                schedulerIndex = model.clickedSchedulerIndex
                schedulerA = List.head (List.filter (\ scheduler -> scheduler.id == schedulerIndex ) oldBoard.schedulers )
                schedulerOld = checkScheduler schedulerA
                taskOldA = List.head (List.filter (\task -> task.id == taskId ) schedulerOld.tasks )
                taskOld = checkTask taskOldA
                taskNew = { taskOld | completed = not taskOld.completed }
                filteredTasks = List.filter (\task -> task.id /= taskId ) schedulerOld.tasks
                schedulerNew = { schedulerOld | tasks = List.sortBy .id ( taskNew :: filteredTasks ) }
                filteredSchedulers = List.filter (\ scheduler -> scheduler.id /= schedulerIndex ) oldBoard.schedulers
                newBoard = { oldBoard | schedulers = List.sortBy .stable_id (schedulerNew :: filteredSchedulers) }
                filteredBoards = List.filter ( \ board -> board.id /= activeBoardIndex ) model.dashboard.boards
                updatedBoards = newBoard :: filteredBoards
                oldDashboard = model.dashboard
                newDashboard = { oldDashboard | boards = updatedBoards }
            in
                ( { model | dashboard = newDashboard }, Cmd.none )
        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDrop
            in
                ( { model
                    | dragDrop = model_
                    , dashboard =
                        case result of
                            Nothing ->
                                model.dashboard

                            Just ( taskId, newSchedulerId, _) ->
                                let
                                    activeBoardIndex = model.selectedBoardIndex
                                    oldBoardA = List.head (List.filter (\ board -> board.id == activeBoardIndex ) model.dashboard.boards )
                                    oldBoard = checkBoard oldBoardA
                                    fromSchedulerIndex = model.clickedSchedulerIndex
                                    fromSchedulerOldA = List.head (List.filter (\ scheduler -> scheduler.id == fromSchedulerIndex ) oldBoard.schedulers )
                                    fromSchedulerOld = checkScheduler fromSchedulerOldA
                                    filteredTasks = List.filter (\ task -> task.id /= taskId ) fromSchedulerOld.tasks
                                    draggedTaskA = List.head (List.filter (\ task -> task.id == taskId ) fromSchedulerOld.tasks )
                                    draggedTask = checkTask draggedTaskA
                                    fromSchedulerNew = { fromSchedulerOld | tasks = filteredTasks }
                                    toSchedulerOldA = List.head (List.filter (\ scheduler -> scheduler.id == newSchedulerId ) oldBoard.schedulers )
                                    toSchedulerOld = checkScheduler toSchedulerOldA
                                    toSchedulerNew = { toSchedulerOld | tasks = List.sortBy .id (draggedTask :: toSchedulerOld.tasks) }
                                    filteredSchedulers = List.filter (\ scheduler -> scheduler.id /= fromSchedulerIndex && scheduler.id /= newSchedulerId ) oldBoard.schedulers
                                    newBoard = { oldBoard | schedulers = List.sortBy .stable_id (fromSchedulerNew :: toSchedulerNew :: filteredSchedulers) }
                                    filteredBoards = List.filter ( \ board -> board.id /= activeBoardIndex ) model.dashboard.boards
                                    updatedBoards = newBoard :: filteredBoards
                                    oldDashboard = model.dashboard
                                    newDashboard = { oldDashboard | boards = updatedBoards }

                                in
                                    newDashboard
                }, Cmd.none )
        SetClickedSchedulerIndex id ->
            ( { model | clickedSchedulerIndex = id }, Cmd.none )
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
                newBoard = { oldBoard | schedulers =
                    List.sortBy .stable_id (( Scheduler title "" [] model.currentIndex "" model.currentIndex ) :: oldBoard.schedulers)
                    , pendingSchedulerTitle = "" }
                filteredBoards = List.filter ( \ board -> board.id /= selectedBoardIndex ) model.dashboard.boards
                updatedBoards = newBoard :: filteredBoards
                oldDashboard = model.dashboard
                newDashboard = { oldDashboard | boards = updatedBoards }
            in
                if String.isEmpty title then
                    ( model, Cmd.none )
                else
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
                updatedBoard = { oldBoard | schedulers = List.sortBy .id (newScheduler :: filteredSchedulers) }
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
                    tasks = List.sortBy .id ( ( Task oldScheduler.pendingTaskTitle False model.currentIndex ) :: oldScheduler.tasks )
                    , pendingTaskTitle = ""
                    }
                filteredSchedulerList = List.filter (\ scheduler -> scheduler.id /= oldBoard.activeSchedulerIndex) oldBoard.schedulers
                updatedSchedulers = List.sortBy .stable_id (newScheduler :: filteredSchedulerList)
                newBoard = { oldBoard | schedulers = updatedSchedulers }
                filteredBoardList = List.filter (\ board -> board.id /= model.selectedBoardIndex) model.dashboard.boards
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



checkTask : Maybe Task -> Task
checkTask a =
    case a of
        Nothing ->
            Task "" False 0
        Just a ->
            Task a.title a.completed a.id

checkScheduler : Maybe Scheduler -> Scheduler
checkScheduler a =
    case a of
        Nothing ->
            Scheduler "" "" [] 0 "" 0
        Just a ->
            Scheduler a.title a.currentTask a.tasks a.id a.pendingTaskTitle a.stable_id


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
    div [ ]
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
    div [ class "boardDetails" ]
        [ div[class "centerBoardDetails"][h1[] [ text board.title ]
        , button [ onClick ActionBack, class "backButton" ] [ i [ class "fa fa-arrow-left" ] [], text " Back "]
        , br [] []
        , input [ placeholder "Enter scheduler title"
            , value board.pendingSchedulerTitle
            , onInput ChangeCurrentSchedulerTitle ] []
        , button [ onClick AddScheduler, class "addButton" ] [ text "Add Scheduler" ]]
        , br [] []
        , ul [ class "horizontalSchedulers" ] ( List.map schedulerView board.schedulers )
        ]


schedulerView : Scheduler -> Html Msg
schedulerView scheduler =
    li [ onMouseDown (SetClickedSchedulerIndex scheduler.id) ]
        [ h4 [] [ text scheduler.title ]
        , input [ onClick (ChangeCurrentSchedulerIndex scheduler.id)
        , onInput ChangeCurrentTaskTitle, placeholder "Title for new task!"
        , value scheduler.pendingTaskTitle ] []
        , button [ onClick AddTask, class "addButton" ] [ text "Add Task" ]
        , ul ( DragDrop.droppable DragDropMsg scheduler.id ) ( List.map taskView scheduler.tasks )
        ]

-- STYLED COMPONENTS

checkedTaskStyle : List Style
checkedTaskStyle =
    [ textDecoration lineThrough ]

taskView : Task -> Html Msg
taskView task =
    li ( DragDrop.draggable DragDropMsg task.id )
        [ ( if ( task.completed ) then span [ style checkedTaskStyle ] [ text task.title ] else span [] [ text task.title ] )
        , input [ type_ "checkbox", checked task.completed, onClick (CheckTask task.id) ] []
        ]



indexView : Model -> Html Msg
indexView model =
    div [ class "myclass" ]
        [ input [ value model.dashboard.currentBoardTitle
            , onInput ChangeCurrentBoardIitle
            , placeholder "Enter board title"
            ] []
        , button [ onClick AddBoard, class "addButton" ] [ text "Add board" ]
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