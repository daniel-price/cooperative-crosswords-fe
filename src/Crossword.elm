module Crossword exposing (Cell(..), CellData, Clue, ClueId, Clues, Crossword(..), Direction(..), Internals, directionToString, fetch, getCells, getClueNumber, getClues, getColumnNumber, getCurrentCellChar, getDirection, getNewDirection, getNextWhiteCell, getNumberOfRows, getRowNumber, getText, updateGrid)

import Constants
import Http
import Json.Decode as D
import Json.Decode.Pipeline as DP
import List.Extra
import Url.Builder
import Util


type Direction
    = Across
    | Down


type Crossword
    = Crossword Internals


type alias Clue =
    { id : Int
    , value : String
    }


type alias Clues =
    { across : List Clue
    , down : List Clue
    }


type alias ClueId =
    { direction : Direction, number : Int }


type Cell
    = White CellData
    | Black


type alias Internals =
    { numberOfColumns : Int
    , numberOfRows : Int
    , clues : Clues
    , grid : List Cell
    }


type alias CellData =
    { clueId1 : ClueId
    , clueId2 : Maybe ClueId
    , value : Maybe Char -- move to state
    , number : Maybe Int
    }


fetch : String -> (Result Http.Error Crossword -> msg) -> Cmd msg
fetch id msg =
    Http.get
        { url =
            Url.Builder.crossOrigin
                Constants.apiUrl
                [ "crossword", id ]
                []
        , expect = Http.expectJson msg crosswordDecoder
        }


crosswordDecoder : D.Decoder Crossword
crosswordDecoder =
    D.succeed Internals
        |> DP.required "numberOfColumns" D.int
        |> DP.required "numberOfRows" D.int
        |> DP.required "clues" cluesDecoder
        |> DP.required "grid" (D.list cellDecoder)
        |> D.map Crossword


cluesDecoder : D.Decoder Clues
cluesDecoder =
    D.succeed Clues
        |> DP.required "across" (D.list clueDecoder)
        |> DP.required "down" (D.list clueDecoder)


clueDecoder : D.Decoder Clue
clueDecoder =
    D.succeed Clue
        |> DP.required "number" D.int
        |> DP.required "value" D.string


cellDecoder : D.Decoder Cell
cellDecoder =
    D.oneOf [ whiteDecoder, blackDecoder ]


whiteDecoder : D.Decoder Cell
whiteDecoder =
    exactMatch (D.field "type" D.string)
        "White"
        (D.map White <| D.field "cellData" decodeCellData)


blackDecoder : D.Decoder Cell
blackDecoder =
    exactMatch (D.field "type" D.string) "Black" (D.succeed Black)


decodeCellData : D.Decoder CellData
decodeCellData =
    D.succeed CellData
        |> DP.required "clueId" decodeClueId
        |> DP.optional "clueId2" (D.map Just decodeClueId) Nothing
        |> DP.optional "value" decodeChar Nothing
        |> DP.optional "number" (D.map Just D.int) Nothing


decodeClueId : D.Decoder ClueId
decodeClueId =
    D.succeed ClueId
        |> DP.required "direction" decodeDirection
        |> DP.required "number" D.int


decodeDirection : D.Decoder Direction
decodeDirection =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "Across" ->
                        D.succeed Across

                    "Down" ->
                        D.succeed Down

                    _ ->
                        D.fail "Invalid Direction"
            )


decodeChar : D.Decoder (Maybe Char)
decodeChar =
    D.string
        |> D.andThen toChar


toChar : String -> D.Decoder (Maybe Char)
toChar string =
    D.succeed (List.head (String.toList string))


exactMatch : D.Decoder String -> String -> D.Decoder a -> D.Decoder a
exactMatch matchDecoder match dec =
    matchDecoder
        |> D.andThen
            (\str ->
                if str == match then
                    dec

                else
                    D.fail <| "[exactMatch] tgt: " ++ match ++ " /= " ++ str
            )


directionToString : Direction -> String
directionToString direction =
    case direction of
        Across ->
            "Across"

        Down ->
            "Down"


getCells : Crossword -> List Cell
getCells (Crossword crossword) =
    crossword.grid


getNumberOfRows : Crossword -> Int
getNumberOfRows (Crossword crossword) =
    crossword.numberOfRows


getClues : Crossword -> Direction -> List Clue
getClues (Crossword crossword) direction =
    case direction of
        Across ->
            crossword.clues.across

        Down ->
            crossword.clues.down


getClueNumber : Clue -> Int
getClueNumber clue =
    clue.id


getText : Clue -> String
getText clue =
    clue.value


getDirection : ClueId -> Direction
getDirection clueId =
    clueId.direction


getRowNumber : Crossword -> Int -> Int
getRowNumber (Crossword crossword) index =
    floor (toFloat index / toFloat crossword.numberOfColumns) + 1


getColumnNumber : Crossword -> Int -> Int
getColumnNumber (Crossword crossword) index =
    modBy crossword.numberOfColumns index + 1


updateGrid : Crossword -> Int -> Maybe Char -> Crossword
updateGrid (Crossword crossword) index newChar =
    Crossword
        { crossword
            | grid =
                crossword.grid
                    |> List.Extra.updateIfIndex ((==) index)
                        (\item ->
                            case item of
                                White cellData ->
                                    White { cellData | value = newChar }

                                Black ->
                                    Black
                        )
        }


getLeftWhiteIndex : List Cell -> Int -> Int
getLeftWhiteIndex grid index =
    let
        previousSquares : List Cell
        previousSquares =
            List.reverse (Tuple.first (List.Extra.splitAt index grid))

        offset : Maybe Int
        offset =
            List.Extra.findIndex isWhiteSquare previousSquares
    in
    case offset of
        Just n ->
            index - n - 1

        Nothing ->
            -- reached the first square
            index


getDownWhiteIndex : Int -> Crossword -> Int
getDownWhiteIndex currentIndex crossword =
    let
        columnSquares : List Cell
        columnSquares =
            Util.takeEveryNthIndexesFromIndex (getNumberOfRows crossword) (getColumnNumber crossword currentIndex) (getCells crossword)

        columnsDown : List Cell
        columnsDown =
            Tuple.second (List.Extra.splitAt (getRowNumber crossword currentIndex) columnSquares)

        index : Maybe Int
        index =
            List.Extra.findIndex isWhiteSquare columnsDown
    in
    case index of
        Just n ->
            currentIndex + (getNumberOfRows crossword * (n + 1))

        Nothing ->
            -- reached the last square
            currentIndex


getUpWhiteIndex : Int -> Crossword -> Int
getUpWhiteIndex currentIndex crossword =
    let
        columnSquares : List Cell
        columnSquares =
            Util.takeEveryNthIndexesFromIndex (getNumberOfRows crossword) (getColumnNumber crossword currentIndex) (getCells crossword)

        columnsUp : List Cell
        columnsUp =
            List.reverse (Tuple.first (List.Extra.splitAt (getRowNumber crossword currentIndex - 1) columnSquares))

        index : Maybe Int
        index =
            List.Extra.findIndex isWhiteSquare columnsUp
    in
    case index of
        Just n ->
            currentIndex - (getNumberOfRows crossword * (n + 1))

        Nothing ->
            -- reached the last square
            currentIndex


getRightWhiteIndex : List Cell -> Int -> Int
getRightWhiteIndex grid index =
    let
        nextSquares : List Cell
        nextSquares =
            Tuple.second (List.Extra.splitAt (index + 1) grid)
    in
    case List.Extra.findIndex isWhiteSquare nextSquares of
        Just n ->
            index + 1 + n

        Nothing ->
            -- reached the last square
            index


isWhiteSquare : Cell -> Bool
isWhiteSquare cell =
    case cell of
        White _ ->
            True

        Black ->
            False


getNextWhiteCell : Crossword -> Direction -> Int -> Bool -> Int
getNextWhiteCell crossword direction index backwards =
    if direction == Across then
        if backwards then
            getLeftWhiteIndex (getCells crossword) index

        else
            getRightWhiteIndex (getCells crossword) index

    else if backwards then
        getUpWhiteIndex index crossword

    else
        getDownWhiteIndex index crossword


getCurrentCellChar : Int -> Crossword -> Maybe Char
getCurrentCellChar index crossword =
    let
        cell : Maybe Cell
        cell =
            Util.elementAtIndex (index + 1) (getCells crossword)
    in
    case cell of
        Just (White cellData) ->
            cellData.value

        _ ->
            Nothing


getNewDirection : Int -> Int -> Direction -> CellData -> Direction
getNewDirection newIndex currentIndex currentDirection cellData =
    let
        cellHasOneDirection : Bool
        cellHasOneDirection =
            case cellData.clueId2 of
                Just _ ->
                    False

                Nothing ->
                    True
    in
    if cellHasOneDirection then
        getDirection cellData.clueId1

    else if currentIndex == newIndex then
        if currentDirection == Across then
            Down

        else
            Across

    else
        currentDirection
