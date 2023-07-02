module Pages.Crossword.Id_ exposing (Model, Msg, page)

import Crossword exposing (Cell(..), Clue, Crossword, Direction(..))
import Gen.Params.Crossword.Id_ exposing (Params)
import Html exposing (Html, div, text)
import Html.Attributes exposing (id, placeholder, style, value)
import Html.Events exposing (onClick)
import Http
import List.Extra
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


type alias State =
    { index : Int
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
    = GotCrossword (Result Http.Error Crossword)
    | CellSelected Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCrossword crossword ->
            case crossword of
                Ok c ->
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

                        state : { index : Int }
                        state =
                            { index = index
                            }
                    in
                    ( Loaded c state, Cmd.none )

                Err e ->
                    ( Error e, Cmd.none )

        CellSelected index ->
            case model of
                Loaded crossword state ->
                    let
                        newState : State
                        newState =
                            { state | index = index }
                    in
                    ( Loaded crossword newState, Cmd.none )

                _ ->
                    ( model, Cmd.none )



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

                Loaded crossword state ->
                    viewPuzzle crossword state
            ]
        ]
    }


viewPuzzle : Crossword -> State -> Html.Html Msg
viewPuzzle crossword state =
    div [ style "display" "flex" ]
        [ viewGrid crossword state
        , viewCluesSection Across crossword
        , viewCluesSection Down crossword
        ]


viewCluesSection : Direction -> Crossword -> Html Msg
viewCluesSection direction crossword =
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
                (List.map viewClue (Crossword.getClues crossword direction))
            ]
        ]


viewClue : Clue -> Html Msg
viewClue clue =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        ]
        [ div
            []
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
        (List.indexedMap (viewCell state) (Crossword.getCells crossword))


viewCell : State -> Int -> Cell -> Html.Html Msg
viewCell state index cell =
    case cell of
        White cellData ->
            let
                isSelected : Bool
                isSelected =
                    state.index == index

                outline : String
                outline =
                    if isSelected then
                        "3px solid DodgerBlue"

                    else
                        "0px"

                border : String
                border =
                    if isSelected then
                        "3px solid DodgerBlue"

                    else
                        "1px solid black"

                zIndex : String
                zIndex =
                    if isSelected then
                        "10"

                    else
                        "0"

                borderWidth : String
                borderWidth =
                    if isSelected then
                        "3px"

                    else
                        "1px"
            in
            div
                [ style "position" "relative"
                , onClick (CellSelected index)
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
                    , style "outline" outline
                    , style "border" border
                    , style "z-index" zIndex
                    , style "border-width" borderWidth
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
