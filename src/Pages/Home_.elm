module Pages.Home_ exposing (Model, Msg, page)

import CrosswordInfo exposing (CrosswordInfo)
import Gen.Params.Home_ exposing (Params)
import Html exposing (div, text)
import Http exposing (Error(..))
import Page
import Request
import Shared
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page _ _ =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type Model
    = Loading
    | Loaded (List CrosswordInfo)
    | Error Http.Error


init : ( Model, Cmd Msg )
init =
    ( Loading
    , CrosswordInfo.fetch GotCrosswordInfoList
    )



-- UPDATE


type Msg
    = GotCrosswordInfoList (Result Http.Error (List CrosswordInfo))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotCrosswordInfoList cil ->
            case cil of
                Ok ci ->
                    ( Loaded ci, Cmd.none )

                Err e ->
                    ( Error e, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Crosswords"
    , body =
        [ case model of
            Loading ->
                div [] [ text "Loading..." ]

            Error e ->
                div [] [ text (errorToString e) ]

            Loaded cil ->
                viewCrosswordInfoList cil
        ]
    }


viewCrosswordInfoList : List CrosswordInfo -> Html.Html Msg
viewCrosswordInfoList crosswordInfoList =
    div []
        (List.map viewCrosswordInfo crosswordInfoList)


viewCrosswordInfo : CrosswordInfo -> Html.Html Msg
viewCrosswordInfo crosswordInfo =
    div []
        [ text (CrosswordInfo.getName crosswordInfo ++ CrosswordInfo.getCrosswordId crosswordInfo)
        ]



-- OTHER


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
