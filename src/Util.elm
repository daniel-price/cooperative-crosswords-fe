module Util exposing (charToString, elementAtIndex, errorToString, takeEveryNthIndexesFromIndex, viewLink)

import Html exposing (a, div)
import Html.Attributes exposing (href)
import Http exposing (Error(..))
import List.Extra


errorToString : Http.Error -> String
errorToString error =
    case error of
        BadUrl url ->
            "The URL " ++ url ++ " was invalid"

        Timeout ->
            "Unable to reach the server, try again"

        NetworkError ->
            "Unable to reach the server, check your network connection"

        BadStatus 500 ->
            "The server had a problem, try again later"

        BadStatus 400 ->
            "Verify your information and try again"

        BadStatus _ ->
            "Unknown error"

        BadBody errorMessage ->
            errorMessage


viewLink : String -> String -> Html.Html msg
viewLink path text =
    div [] [ a [ href path ] [ Html.text text ] ]


charToString : Maybe Char -> String
charToString char =
    case char of
        Just c ->
            String.fromChar c

        Nothing ->
            ""


elementAtIndex : Int -> List a -> Maybe a
elementAtIndex index list =
    if List.length list >= index then
        List.take index list
            |> List.reverse
            |> List.head

    else
        Nothing


takeEveryNthIndexesFromIndex : Int -> Int -> List a -> List a
takeEveryNthIndexesFromIndex n initialIndex l =
    let
        cellsFromIndex : List a
        cellsFromIndex =
            Tuple.second (List.Extra.splitAt (initialIndex - 1) l)
    in
    cellsFromIndex
        |> List.indexedMap
            (\i x ->
                if (i |> modBy n) == 0 then
                    Just x

                else
                    Nothing
            )
        |> List.filterMap identity
