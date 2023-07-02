module Pages.Home_ exposing (Model, Msg, page)

import Gen.Params.Home_ exposing (Params)
import Html exposing (div, text)
import Page
import Request
import Shared
import Task
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


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}
    , Task.perform (always NoOp) (Task.succeed ())
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view _ =
    { title = "Crosswords"
    , body =
        [ div [] [ text "Crosswords" ]
        ]
    }

