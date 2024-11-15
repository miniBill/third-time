port module Main exposing (main)

import Audio exposing (AudioCmd)
import Browser
import Browser.Events
import Duration exposing (Duration)
import Element exposing (Element, centerX, column, el, fill, height, padding, spacing, text, width)
import Element.Background as Background
import Json.Decode
import Json.Encode
import Quantity
import Time


type alias Flags =
    {}


type alias Model =
    { workTime : Duration
    , fraction : Int
    , currentState : State
    , now : Time.Posix
    , sound : SoundState
    }


type SoundState
    = SoundLoading
    | SoundError Audio.LoadError
    | SoundLoaded Audio.Source


type State
    = Stopped
    | Working
        { from : Time.Posix
        , bankedBreakTime : Duration
        }
    | OnBreak { endsOn : Time.Posix }


type Msg
    = StartBreak
    | StartWork
    | Stop
    | Tick Time.Posix
    | SoundLoadResult (Result Audio.LoadError Audio.Source)


main : Program Flags (Audio.Model Msg (Maybe Model)) (Audio.Msg Msg)
main =
    Audio.documentWithAudio
        { init = init
        , view = outerView
        , update = update
        , subscriptions = subscriptions
        , audio = audio
        , audioPort = { toJS = audioPortToJS, fromJS = audioPortFromJS }
        }


port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


audio : audioData -> Maybe Model -> Audio.Audio
audio _ maybeModel =
    case maybeModel of
        Nothing ->
            -- https://www.youtube.com/watch?v=aGSKrC7dGcY
            Audio.silence

        Just { currentState, sound } ->
            case ( currentState, sound ) of
                ( OnBreak { endsOn }, SoundLoaded source ) ->
                    Audio.audio source endsOn

                _ ->
                    Audio.silence


outerView : audioData -> Maybe Model -> Browser.Document Msg
outerView _ maybeModel =
    case maybeModel of
        Nothing ->
            { title = "Loading...", body = [] }

        Just model ->
            { title = title model
            , body =
                [ Element.layout
                    [ width fill
                    , height fill
                    ]
                    (view model)
                ]
            }


title : Model -> String
title { now, currentState } =
    case currentState of
        Stopped ->
            "Stopped"

        Working { from } ->
            "Working"
                ++ ": "
                ++ formatDuration (Duration.from from now)

        OnBreak { endsOn } ->
            "On break"
                ++ ": "
                ++ formatDuration (Duration.from now endsOn)


formatDuration : Duration -> String
formatDuration duration =
    let
        seconds : Int
        seconds =
            Duration.inSeconds duration
                |> round
    in
    if seconds < 0 then
        "-" ++ formatDuration (Quantity.negate duration)

    else
        let
            f n =
                String.padLeft 2 '0' (String.fromInt n)

            mmSs =
                f (seconds // 60 |> modBy 60) ++ ":" ++ f (seconds |> modBy 60)
        in
        if seconds < 3600 then
            mmSs

        else
            f (seconds // 3600) ++ ":" ++ mmSs


init : flags -> ( Maybe Model, Cmd msg, AudioCmd Msg )
init _ =
    ( Nothing
    , Cmd.none
    , Audio.loadAudio SoundLoadResult "/src/Wakka%20Wakka.mp3"
    )


view : Model -> Element Msg
view model =
    column
        [ width fill
        , height fill
        , spacing 8
        , centerX
        , padding 8
        , Background.color
            (case model.currentState of
                Stopped ->
                    Element.rgb255 0x45 0x54 0xA1

                Working _ ->
                    Element.rgb255 0x29 0xA3 0x3D

                OnBreak _ ->
                    Element.rgb255 0xFF 0x95 0x00
            )
        ]
        [ case model.sound of
            SoundLoading ->
                text "Loading audio..."

            SoundError _ ->
                text "Error loading audio 😔"

            SoundLoaded _ ->
                Element.none
        , text "WIP"
        ]


update : audioData -> Msg -> Maybe Model -> ( Maybe Model, Cmd msg, AudioCmd msg )
update _ msg maybeModel =
    case ( msg, maybeModel ) of
        ( Tick now, Nothing ) ->
            ( { workTime = Quantity.zero
              , fraction = 3
              , currentState = Stopped
              , now = now
              , sound = SoundLoading
              }
                |> Just
            , Cmd.none
            , Audio.cmdNone
            )

        ( Tick now, Just model ) ->
            ( Just { model | now = now }, Cmd.none, Audio.cmdNone )

        ( _, Nothing ) ->
            ( Nothing, Cmd.none, Audio.cmdNone )

        ( StartBreak, Just _ ) ->
            Debug.todo "branch '( StartBreak, Just _ )' not implemented"

        ( StartWork, Just _ ) ->
            Debug.todo "branch '( StartWork, Just _ )' not implemented"

        ( Stop, Just _ ) ->
            Debug.todo "branch '( Stop, Just _ )' not implemented"

        ( SoundLoadResult sound, Just model ) ->
            ( Just
                { model
                    | sound =
                        case sound of
                            Ok source ->
                                SoundLoaded source

                            Err err ->
                                SoundError err
                }
            , Cmd.none
            , Audio.cmdNone
            )


subscriptions : audioData -> model -> Sub Msg
subscriptions _ _ =
    Browser.Events.onAnimationFrame Tick
