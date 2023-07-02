module Util exposing (errorToString, viewLink)

import Html exposing (a, div)
import Html.Attributes exposing (href)
import Http exposing (Error(..))


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
