module Pages.Crossword.Id_ exposing (Model, Msg, page)

import Crossword exposing (Crossword)
import Gen.Params.Crossword.Id_ exposing (Params)
import Html exposing (div, text)
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
                    Html.text (Crossword.toString crossword)
            ]
        ]
    }
