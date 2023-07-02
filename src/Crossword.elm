module Crossword exposing (Cell(..), CellData, Clue, ClueId, Clues, Crossword(..), Direction(..), Internals, fetch, getCells, getNumberOfRows)

import Constants
import Http
import Json.Decode as D
import Json.Decode.Pipeline as DP
import Url.Builder


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


getCells : Crossword -> List Cell
getCells (Crossword crossword) =
    crossword.grid


getNumberOfRows : Crossword -> Int
getNumberOfRows (Crossword crossword) =
    crossword.numberOfRows
