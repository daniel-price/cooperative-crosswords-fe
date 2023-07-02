module Pages.Crossword.Id_ exposing (Model, Msg, page)

import Crossword exposing (Cell(..), Crossword)
import Gen.Params.Crossword.Id_ exposing (Params)
import Html exposing (div, text)
import Html.Attributes exposing (id, placeholder, style, value)
import Http
import Page
import Request
import Shared
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


type Model
    = Loading
    | Loaded Crossword
    | Error Http.Error


init : Request.With Params -> ( Model, Cmd Msg )
init req =
    ( Loading
    , Crossword.fetch req.params.id GotCrossword
    )



-- UPDATE


type Msg
    = GotCrossword (Result Http.Error Crossword)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotCrossword crossword ->
            case crossword of
                Ok c ->
                    ( Loaded c, Cmd.none )

                Err e ->
                    ( Error e, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



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

                Loaded crossword ->
                    viewPuzzle crossword
            ]
        ]
    }


viewPuzzle : Crossword -> Html.Html Msg
viewPuzzle crossword =
    div [ style "display" "flex" ]
        [ viewGrid crossword
        ]


viewGrid : Crossword -> Html.Html Msg
viewGrid crossword =
    div
        [ style "border" "1px solid black"
        , style "display" "grid"
        , style "height" "750px"
        , style "width" "750px"
        , style "padding" "0"
        , style "margin" "0"
        , style "grid-template" (getGridTemplate crossword)
        , style "list-style-type" "none"
        , style "position" "relative"
        ]
        (List.indexedMap viewCell (Crossword.getCells crossword))


viewCell : Int -> Cell -> Html.Html Msg
viewCell index cell =
    case cell of
        White cellData ->
            div
                [ style "position" "relative"
                ]
                [ div
                    [ style "position" "absolute"
                    , style "z-index" "20"
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
                    [ id (String.fromInt index)
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
                    , style "width" "50px"
                    , style "height" "50px"
                    , style "border" "1px solid black"
                    ]
                    [ text (Util.charToString cellData.value) ]
                ]

        Black ->
            div
                [ style "background-color" "black"
                ]
                []



-- OTHER


getGridTemplate : Crossword -> String
getGridTemplate crossword =
    let
        singleCellPercentage : Float
        singleCellPercentage =
            getCellPercentage crossword
    in
    String.concat [ "repeat(", String.fromInt (Crossword.getNumberOfRows crossword), ", ", String.fromFloat singleCellPercentage, "%)/repeat(", String.fromInt (Crossword.getNumberOfRows crossword), ", ", String.fromFloat singleCellPercentage, "%)" ]


getCellPercentage : Crossword -> Float
getCellPercentage crossword =
    100 / toFloat (Crossword.getNumberOfRows crossword)
