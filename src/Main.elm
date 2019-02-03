module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Task
import TextInput
import Time



-- MODEL


type alias Model =
    { time : Int
    , targetTime : Int
    , content : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = 0
      , targetTime = 0
      , content = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | Change String
    | Submit (Maybe Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if model.targetTime == 0 then
                ( model
                , Cmd.none
                )

            else
                ( { model | time = model.time + 1000 }
                , Cmd.none
                )

        Change newContent ->
            ( { model | content = newContent }
            , Cmd.none
            )

        Submit content ->
            case content of
                Just minutes ->
                    ( { model | targetTime = minutes * 60 * 1000, time = 0, content = "" }
                    , Cmd.none
                    )

                Nothing ->
                    ( model
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
    let
        formatHour =
            if String.length hour == 1 then
                "0" ++ hour

            else
                hour

        formatMinute =
            if String.length minute == 1 then
                "0" ++ minute

            else
                minute

        formatSecond =
            if String.length second == 1 then
                "0" ++ second

            else
                second
    in
    { title = "Elm Time Keeper"
    , body =
        [ viewTimer formatHour formatMinute formatSecond
        , viewInput model.content
        ]
    }


viewTimer : String -> String -> String -> Html Msg
viewTimer hour minute second =
    h1 [] [ text (hour ++ ":" ++ minute ++ ":" ++ second) ]


viewInput : String -> Html Msg
viewInput content =
    div []
        [ input [ placeholder "Input the minutes", value content, onInput Change ] []
        , button [ onClick (Submit (String.toInt content)) ] [ text "Start" ]
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
