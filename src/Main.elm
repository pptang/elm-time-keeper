module Main exposing (main)

import Browser exposing (Document)
import Html exposing (..)
import Task
import Time



-- MODEL


type alias Model =
    { time : Int
    , targetTime : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0
      , targetTime = 10 * 60 * 1000
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | time = model.time + 1000 }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> Document Msg
view model =
    let
        hour =
            String.fromInt (Time.toHour Time.utc (Time.millisToPosix (model.targetTime - model.time)))

        minute =
            String.fromInt (Time.toMinute Time.utc (Time.millisToPosix (model.targetTime - model.time)))

        second =
            String.fromInt (Time.toSecond Time.utc (Time.millisToPosix (model.targetTime - model.time)))
    in
    { title = "Elm Time Keeper"
    , body = [ viewTimer hour minute second ]
    }


viewTimer : String -> String -> String -> Html Msg
viewTimer hour minute second =
    h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
