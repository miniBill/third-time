port module Main exposing (main)

import Audio exposing (AudioCmd)
import Browser
import Browser.Events
import Duration exposing (Duration)
import Element exposing (Element, alignBottom, alignRight, centerX, centerY, column, el, fill, height, padding, row, shrink, spacing, table, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Json.Decode
import Json.Encode
import Quantity
import Time


type alias Flags =
    {}


type alias Model =
    { workTime : Duration
    , divisor : Float
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
        { startedFrom : Time.Posix
        , bankedBreakTime : Duration
        }
    | OnBreak { endsOn : Time.Posix }


type Msg
    = StartBreak
    | StartWork
    | Stop
    | Reset
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

        Working { startedFrom } ->
            "Working: " ++ durationToString (Duration.from startedFrom now)

        OnBreak { endsOn } ->
            "On break: " ++ durationToString (Duration.from now endsOn)


durationToString : Duration -> String
durationToString duration =
    let
        seconds : Int
        seconds =
            Duration.inSeconds duration
                |> round
    in
    if seconds < 0 then
        "-" ++ durationToString (Quantity.negate duration)

    else
        let
            f : Int -> String
            f n =
                String.padLeft 2 '0' (String.fromInt n)

            mmSs : String
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
    , Audio.loadAudio SoundLoadResult "/src/Chime.mp3"
    )


fontSize =
    { main = Font.size 32
    , small = Font.size 24
    }


view : Model -> Element Msg
view model =
    column
        [ width fill
        , height fill
        , spacing 8
        , centerX
        , padding 8
        , fontSize.main
        , Font.color (Element.rgb255 0xFF 0xFF 0xFF)
        , Background.color
            (case model.currentState of
                Stopped ->
                    Element.rgb255 0x45 0x54 0xA1

                Working _ ->
                    Element.rgb255 0x29 0xA3 0x3D

                OnBreak _ ->
                    Element.rgb255 0xFF 0x95 0x00
            )
        , Element.inFront (el [ alignRight, padding 8 ] (button Reset "🔄 Reset"))
        ]
        [ el [ centerX ] <|
            case model.sound of
                SoundLoading ->
                    text "Loading audio..."

                SoundError _ ->
                    text "Error loading audio 😔"

                SoundLoaded _ ->
                    Element.none
        , el
            [ centerX
            , centerY
            ]
          <|
            table []
                { data = centralBlock model
                , columns =
                    [ { width = shrink
                      , header = Element.none
                      , view = Tuple.first
                      }
                    , { width = shrink
                      , header = Element.none
                      , view = Tuple.second
                      }
                    ]
                }
        , buttons
        ]


centralBlock : Model -> List ( Element msg, Element msg )
centralBlock model =
    let
        firstLine : Duration -> String -> ( Element msg, Element msg )
        firstLine duration label =
            ( el
                [ Font.alignRight
                , padding 4
                ]
                (text (durationToString duration))
            , el
                [ padding 4
                ]
                (text label)
            )

        line : Duration -> String -> ( Element msg, Element msg )
        line duration label =
            ( el
                [ fontSize.small
                , Font.alignRight
                , padding 4
                ]
                (text (durationToString duration))
            , el
                [ fontSize.small
                , padding 4
                ]
                (text label)
            )
    in
    case model.currentState of
        Stopped ->
            [ firstLine model.workTime "Worked" ]

        Working { startedFrom, bankedBreakTime } ->
            let
                durationWorked : Duration
                durationWorked =
                    Duration.from startedFrom model.now
            in
            [ firstLine durationWorked "Worked"
            , line
                (durationWorked
                    |> Quantity.divideBy model.divisor
                    |> Quantity.plus bankedBreakTime
                )
                "Rest"
            , line (model.workTime |> Quantity.plus durationWorked) "Worked of 4:13 goal"
            ]

        OnBreak { endsOn } ->
            [ firstLine (Duration.from model.now endsOn) "Rest"
            , line model.workTime "Worked of 4:13 goal"
            ]


buttons : Element Msg
buttons =
    row
        [ spacing 8
        , width fill
        , alignBottom
        ]
        [ button StartWork "🛠️ Work"
        , button StartBreak "🕺 Break"
        , button Stop "🔨 Stop"
        ]


button : msg -> String -> Element msg
button msg label =
    Input.button
        [ centerX
        , padding 8
        , Border.width 1
        , Border.rounded 8
        , Background.color (Element.rgba255 0xFF 0xFF 0xFF 0.3)
        ]
        { onPress = Just msg
        , label = text label
        }


update : audioData -> Msg -> Maybe Model -> ( Maybe Model, Cmd msg, AudioCmd msg )
update _ msg maybeModel =
    case maybeModel of
        Nothing ->
            case msg of
                Tick now ->
                    ( { workTime = Quantity.zero
                      , divisor = 3
                      , currentState = Stopped
                      , now = now
                      , sound = SoundLoading
                      }
                        |> Just
                    , Cmd.none
                    , Audio.cmdNone
                    )

                _ ->
                    ( Nothing, Cmd.none, Audio.cmdNone )

        Just model ->
            let
                newModel : Model
                newModel =
                    case msg of
                        Tick now ->
                            { model | now = now }

                        StartBreak ->
                            case model.currentState of
                                Stopped ->
                                    { model
                                        | currentState =
                                            OnBreak
                                                { endsOn = model.now
                                                }
                                    }

                                Working { startedFrom, bankedBreakTime } ->
                                    let
                                        durationWorked : Duration
                                        durationWorked =
                                            Duration.from startedFrom model.now
                                    in
                                    { model
                                        | currentState =
                                            OnBreak
                                                { endsOn =
                                                    durationWorked
                                                        |> Quantity.divideBy model.divisor
                                                        |> Quantity.plus bankedBreakTime
                                                        |> Duration.addTo model.now
                                                }
                                        , workTime =
                                            model.workTime
                                                |> Quantity.plus durationWorked
                                    }

                                OnBreak _ ->
                                    model

                        StartWork ->
                            case model.currentState of
                                Stopped ->
                                    { model
                                        | currentState =
                                            Working
                                                { bankedBreakTime = Quantity.zero
                                                , startedFrom = model.now
                                                }
                                    }

                                Working _ ->
                                    model

                                OnBreak { endsOn } ->
                                    { model
                                        | currentState =
                                            Working
                                                { bankedBreakTime = Duration.from model.now endsOn
                                                , startedFrom = model.now
                                                }
                                    }

                        Stop ->
                            case model.currentState of
                                Stopped ->
                                    model

                                Working { startedFrom } ->
                                    let
                                        durationWorked : Duration
                                        durationWorked =
                                            Duration.from startedFrom model.now
                                    in
                                    { model
                                        | currentState = Stopped
                                        , workTime =
                                            model.workTime
                                                |> Quantity.plus durationWorked
                                    }

                                OnBreak _ ->
                                    { model | currentState = Stopped }

                        Reset ->
                            { model
                                | currentState = Stopped
                                , workTime = Quantity.zero
                            }

                        SoundLoadResult sound ->
                            { model
                                | sound =
                                    case sound of
                                        Ok source ->
                                            SoundLoaded source

                                        Err err ->
                                            SoundError err
                            }
            in
            ( Just newModel, Cmd.none, Audio.cmdNone )


subscriptions : audioData -> model -> Sub Msg
subscriptions _ _ =
    Browser.Events.onAnimationFrame Tick
