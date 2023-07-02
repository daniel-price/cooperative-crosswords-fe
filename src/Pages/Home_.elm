module Pages.Home_ exposing (Model, Msg, page)

import CrosswordInfo exposing (CrosswordInfo)
import Gen.Params.Home_ exposing (Params)
import Html exposing (div, text)
import Http
import Page
import Request
import Shared
import Util exposing (errorToString, viewLink)
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
        [ viewLink ("/crossword/" ++ CrosswordInfo.getCrosswordId crosswordInfo) (CrosswordInfo.getName crosswordInfo)
        ]
