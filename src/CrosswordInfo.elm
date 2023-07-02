module CrosswordInfo exposing (CrosswordInfo, fetch, getCrosswordId, getName)

import Http
import Json.Decode as D
import Json.Decode.Pipeline as DP
import Url.Builder


type CrosswordInfo
    = CrosswordInfo Internals


type CrosswordId
    = CrosswordId String


type alias Internals =
    { id : CrosswordId
    , date : Int
    , series : String
    , seriesNo : Int
    }


fetch : (Result Http.Error (List CrosswordInfo) -> msg) -> Cmd msg
fetch msg =
    Http.get
        { url =
            Url.Builder.crossOrigin
                "https://cooperative-crosswords.onrender.com"
                [ "crosswords" ]
                []
        , expect = Http.expectJson msg (D.list crosswordInfoDecoder)
        }


crosswordInfoDecoder : D.Decoder CrosswordInfo
crosswordInfoDecoder =
    D.succeed Internals
        |> DP.required "id" (D.string |> D.map CrosswordId)
        |> DP.required "date" D.int
        |> DP.required "series" D.string
        |> DP.required "seriesNo" D.int
        |> D.map CrosswordInfo


getName : CrosswordInfo -> String
getName (CrosswordInfo crosswordInfo) =
    crosswordInfo.series ++ ": " ++ String.fromInt crosswordInfo.seriesNo


getCrosswordId : CrosswordInfo -> String
getCrosswordId (CrosswordInfo crosswordInfo) =
    let
        (CrosswordId id) =
            crosswordInfo.id
    in
    id
