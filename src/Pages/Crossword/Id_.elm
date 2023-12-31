module Pages.Crossword.Id_ exposing (Model, Msg, page)

import Browser.Dom as Dom
import Browser.Events
import Crossword exposing (Cell(..), CellData, Clue, ClueId, Crossword, Direction(..))
import Gen.Params.Crossword.Id_ exposing (Params)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (id, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import List.Extra
import Page
import Request
import Shared
import Task
import Util
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page _ req =
    Page.element
        { init = init req
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias State =
    { index : Int
    , direction : Direction
    , clueId : ClueId
    }


type Model
    = Loading
    | Loaded Crossword State
    | Error Http.Error


init : Request.With Params -> ( Model, Cmd Msg )
init req =
    ( Loading
    , Crossword.fetch req.params.id GotCrossword
    )



-- UPDATE


type Msg
    = NoOp
    | GotCrossword (Result Http.Error Crossword)
    | PuzzleChange PuzzleMsg


type PuzzleMsg
    = CellSelected Int CellData
    | CellUnSelected
    | CellChanged Int (Maybe Char)
    | KeyTouched KeyEventMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( NoOp, _ ) ->
            ( model, Cmd.none )

        ( GotCrossword crossword, _ ) ->
            case crossword of
                Ok c ->
                    initPuzzle c

                Err e ->
                    ( Error e, Cmd.none )

        ( PuzzleChange puzzleMsg, Loaded crossword state ) ->
            updatePuzzle puzzleMsg crossword state

        _ ->
            ( model, Cmd.none )


initPuzzle : Crossword -> ( Model, Cmd Msg )
initPuzzle c =
    let
        index : Int
        index =
            Maybe.withDefault
                0
                (List.Extra.findIndex
                    (\cell ->
                        case cell of
                            White _ ->
                                True

                            Black ->
                                False
                    )
                    (Crossword.getCells c)
                )

        cellAtIndex : Maybe Cell
        cellAtIndex =
            List.Extra.getAt index (Crossword.getCells c)

        direction : Direction
        direction =
            case cellAtIndex of
                Just (White cellData) ->
                    Crossword.getDirection cellData.clueId1

                _ ->
                    Across

        clueId : ClueId
        clueId =
            case cellAtIndex of
                Just (White cellData) ->
                    cellData.clueId1

                _ ->
                    { direction = Across, number = 1 }

        state : State
        state =
            { index = index
            , direction = direction
            , clueId = clueId
            }
    in
    ( Loaded c state, focusTextInput )


updatePuzzle : PuzzleMsg -> Crossword -> State -> ( Model, Cmd Msg )
updatePuzzle msg crossword state =
    case msg of
        CellSelected cellIndex cellData ->
            let
                newDirection : Direction
                newDirection =
                    Crossword.getNewDirection cellIndex state.index state.direction cellData

                newClueId : ClueId
                newClueId =
                    case cellData.clueId2 of
                        Just clueId ->
                            if clueId.direction == newDirection then
                                clueId

                            else
                                cellData.clueId1

                        Nothing ->
                            cellData.clueId1

                newState : State
                newState =
                    { index = cellIndex, direction = newDirection, clueId = newClueId }
            in
            ( Loaded crossword newState, focusTextInput )

        CellChanged index string ->
            let
                nextIndex : Int
                nextIndex =
                    Crossword.getNextWhiteCell crossword state.direction state.index False

                newState : State
                newState =
                    { state | index = nextIndex }

                newCrossword : Crossword
                newCrossword =
                    Crossword.updateGrid crossword index string
            in
            ( Loaded newCrossword newState, focusTextInput )

        KeyTouched keyEventMsg ->
            case keyEventMsg of
                BackspacePressed ->
                    let
                        currentCellChar : Maybe Char
                        currentCellChar =
                            Crossword.getCurrentCellChar state.index crossword

                        nextIndex : Int
                        nextIndex =
                            case currentCellChar of
                                Nothing ->
                                    Crossword.getNextWhiteCell crossword state.direction state.index True

                                _ ->
                                    state.index

                        newState : State
                        newState =
                            { state | index = nextIndex }

                        newCrossword : Crossword
                        newCrossword =
                            Crossword.updateGrid crossword state.index Nothing
                    in
                    ( Loaded newCrossword newState, focusTextInput )

                --TODO implement other keys
                _ ->
                    ( Loaded crossword state, Cmd.none )

        CellUnSelected ->
            let
                newState : State
                newState =
                    { state | index = -100, clueId = { direction = Across, number = -100 } }
            in
            ( Loaded crossword newState, focusTextInput )



-- SUBSCRIPTIONS


type KeyEventMsg
    = KeyEventUnknown
    | TabPressed
    | BackspacePressed
    | ShiftPressed
    | ShiftReleased
    | LeftPressed
    | RightPressed
    | UpPressed
    | DownPressed


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (keyDecoder keyDownMap)
        , Browser.Events.onKeyUp (keyDecoder keyUpMap)
        ]


keyDecoder : (String -> KeyEventMsg) -> Decode.Decoder Msg
keyDecoder map =
    let
        createPuzzleChange : KeyEventMsg -> Msg
        createPuzzleChange value =
            PuzzleChange (KeyTouched value)
    in
    Decode.map
        (map >> createPuzzleChange)
        (Decode.field "key" Decode.string)


keyUpMap : String -> KeyEventMsg
keyUpMap eventKeyString =
    case eventKeyString of
        "Shift" ->
            ShiftReleased

        _ ->
            KeyEventUnknown


keyDownMap : String -> KeyEventMsg
keyDownMap eventKeyString =
    case eventKeyString of
        "Shift" ->
            ShiftPressed

        "Tab" ->
            TabPressed

        "Backspace" ->
            BackspacePressed

        "ArrowLeft" ->
            LeftPressed

        "ArrowRight" ->
            RightPressed

        "ArrowUp" ->
            UpPressed

        "ArrowDown" ->
            DownPressed

        _ ->
            KeyEventUnknown



-- VIEW


view : Model -> View Msg
view model =
    { title = "Crossword"
    , body =
        [ div []
            [ case model of
                Loading ->
                    div [] [ text "Loading..." ]

                Error e ->
                    div [] [ text (Util.errorToString e) ]

                Loaded crossword state ->
                    viewPuzzle crossword state
            ]
        ]
    }


viewPuzzle : Crossword -> State -> Html.Html Msg
viewPuzzle crossword state =
    div
        [ style "display" "flex"
        , style "flex-wrap" "wrap"
        ]
        [ background
        , viewGrid crossword state
        , viewCluesSection state Across crossword
        , viewCluesSection state Down crossword
        ]


background : Html Msg
background =
    div
        [ style "height" "100%"
        , style "width" "100%"
        , style "position" "fixed"
        , onClick (PuzzleChange CellUnSelected)
        ]
        []


viewCluesSection : State -> Direction -> Crossword -> Html Msg
viewCluesSection state direction crossword =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        ]
        [ div
            []
            [ text (Crossword.directionToString direction)
            , div
                [ style "display" "flex"
                , style "flex-direction" "column"
                ]
                (List.map (viewClue state direction) (Crossword.getClues crossword direction))
            ]
        ]


viewClue : State -> Direction -> Clue -> Html Msg
viewClue state direction clue =
    let
        backgroundColor : String
        backgroundColor =
            if state.clueId.number == Crossword.getClueNumber clue && direction == state.clueId.direction then
                "yellow"

            else
                "white"
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "background-color" backgroundColor
        ]
        [ div
            [ style "font-weight" "bold"
            , style "padding-right" "10px"
            , style "width" "10px"
            ]
            [ text (String.fromInt (Crossword.getClueNumber clue))
            ]
        , div
            []
            [ text (Crossword.getText clue)
            ]
        ]


viewGrid : Crossword -> State -> Html.Html Msg
viewGrid crossword state =
    div
        [ style "display" "flex"
        , style "flex-wrap" "wrap"
        , style "width" "100vmin"
        , style "max-width" "750px"
        , style "height" "100vmin"
        , style "max-height" "750px"
        , style "padding" "0"
        , style "margin" "0"
        , style "list-style-type" "none"
        , style "position" "relative"
        , style "aspect-ratio" "1"
        , style "align-content" "flex-start"
        ]
        (viewInput crossword state
            :: List.indexedMap (viewCell state) (Crossword.getCells crossword)
        )


viewInput : Crossword -> State -> Html Msg
viewInput crossword state =
    let
        rowNumber : Int
        rowNumber =
            Crossword.getRowNumber crossword state.index

        columnNumber : Int
        columnNumber =
            Crossword.getColumnNumber crossword state.index
    in
    input
        [ id "text-input"
        , style
            "position"
            "absolute"
        , style "z-index" "0"
        , style "background-color" "transparent"
        , style "font-size" "24px" -- prevents zoom on mobile devices
        , style "height" "48px"
        , style "width" "48px"
        , style "outline-width" "0"
        , style "outline" "none"
        , style "border" "none"
        , style "-webkit-box-shadow" "none"
        , style "-moz-box-shadow" "none"
        , style "box-shadow" "none"
        , style "top" (String.concat [ String.fromInt ((rowNumber - 1) * 50), "px" ])
        , style "left" (String.concat [ String.fromInt ((columnNumber - 1) * 50), "px" ])
        , style "border-width" "3px"
        , onInput (onTextInput state)
        , value ""
        ]
        []


onTextInput : State -> String -> Msg
onTextInput state string =
    PuzzleChange (CellChanged state.index (List.head (List.reverse (String.toList string))))


shouldHighlight : State -> CellData -> Bool
shouldHighlight state cellData =
    case cellData.clueId2 of
        Just clueId ->
            (clueId == state.clueId && clueId.direction == state.direction) || cellData.clueId1 == state.clueId

        Nothing ->
            cellData.clueId1 == state.clueId


viewCell : State -> Int -> Cell -> Html.Html Msg
viewCell state index cell =
    case cell of
        White cellData ->
            let
                isHighlighted : Bool
                isHighlighted =
                    shouldHighlight state cellData

                isSelected : Bool
                isSelected =
                    state.index == index

                backgroundColor : String
                backgroundColor =
                    if isHighlighted then
                        "yellow"

                    else
                        "white"

                outline : String
                outline =
                    if isSelected then
                        "3px solid DodgerBlue"

                    else
                        "0px"

                border : String
                border =
                    if isSelected then
                        "1px solid DodgerBlue"

                    else
                        "1px solid black"

                zIndex : String
                zIndex =
                    if isSelected then
                        "10"

                    else
                        "0"
            in
            div
                [ style "display" "relative"
                , onClick (PuzzleChange (CellSelected index cellData))
                , id (String.fromInt index)
                , style
                    "position"
                    "relative"
                , placeholder ""
                , value (Util.charToString cellData.value)
                , style "text-transform" "uppercase"
                , style "box-sizing" "border-box"
                , style "outline" "none"
                , style "text-align" "center"
                , style "font-size" "20px"
                , style "font-weight" "bold"
                , style "background" "transparent"
                , style "width" "6%"
                , style "height" "0px"
                , style "flex-basis" "6.66666666667%"
                , style "padding-bottom" "6%"
                , style "outline" outline
                , style "border" border
                , style "z-index" zIndex
                , style "backgroundColor" backgroundColor
                , style "font-size" "min(4vw,30px)"
                , style "user-select" "none"
                ]
                [ div
                    [ style "position" "absolute"
                    , style "z-index" "20"
                    , style "font-size" "min(2.5vw,17px)"
                    , style "font-weight" "normal"
                    ]
                    [ text
                        (case cellData.number of
                            Just number ->
                                String.fromInt number

                            Nothing ->
                                ""
                        )
                    ]
                , div
                    [ style "line-height" "1.5"
                    ]
                    [ text (Util.charToString cellData.value)
                    ]
                ]

        Black ->
            div
                [ style "position" "relative"
                , id (String.fromInt index)
                , style
                    "position"
                    "relative"
                , placeholder ""
                , style "text-transform" "uppercase"
                , style "box-sizing" "border-box"
                , style "outline" "none"
                , style "text-align" "center"
                , style "font-size" "20px"
                , style "font-weight" "bold"
                , style "background" "transparent"
                , style "width" "6%"
                , style "height" "0px"
                , style "padding-bottom" "6%"
                , style "outline" "0px"
                , style "border" "1px solid black"
                , style "z-index" "0"
                , style "border-width" "1px"
                , style "backgroundColor" "black"
                , style "flex-basis" "6.66666666667%"
                ]
                []



-- OTHER


focusTextInput : Cmd Msg
focusTextInput =
    Task.attempt
        (\_ -> NoOp)
        (Dom.focus "text-input")
