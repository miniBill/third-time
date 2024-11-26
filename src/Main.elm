port module Main exposing (main)

import Audio exposing (AudioCmd)
import Browser
import Browser.Events
import Duration exposing (Duration)
import Element exposing (Element, alignBottom, centerX, column, el, fill, fillPortion, height, moveUp, padding, px, row, shrink, spacing, table, text, width)
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
    , divisor : { string : String, float : Float }
    , currentState : State
    , now : Time.Posix
    , sound : SoundState
    , goal : Duration
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
    | Resting { endsOn : Time.Posix }


type Msg
    = StartBreak
    | StartWork
    | Stop
    | Reset
    | Tick Time.Posix
    | SoundLoadResult (Result Audio.LoadError Audio.Source)
    | Divisor String


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
                ( Resting { endsOn }, SoundLoaded source ) ->
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
                    (containerStyle model)
                    (view model)
                ]
            }


containerStyle : Model -> List (Element.Attribute Msg)
containerStyle model =
    [ width fill
    , height fill
    , spacing 8
    , padding 8
    , fontSize.main
    , Font.color (Element.rgb255 0xFF 0xFF 0xFF)
    , Background.color (backgroundColor model)
    ]


backgroundColor : Model -> Element.Color
backgroundColor model =
    case model.currentState of
        Stopped ->
            Element.rgb255 0x45 0x54 0xA1

        Working _ ->
            Element.rgb255 0x29 0xA3 0x3D

        Resting { endsOn } ->
            if Time.posixToMillis model.now < Time.posixToMillis endsOn then
                Element.rgb255 0xFF 0x95 0x00

            else
                Element.rgb255 0x8F 0x55 0x00


title : Model -> String
title { now, currentState } =
    case currentState of
        Stopped ->
            "Stopped"

        Working { startedFrom } ->
            "Working: " ++ durationToString (Duration.from startedFrom now)

        Resting { endsOn } ->
            if Time.posixToMillis now < Time.posixToMillis endsOn then
                "Resting: " ++ durationToString (Duration.from now endsOn)

            else
                "🔔 Resting: " ++ durationToString (Duration.from now endsOn)


durationToString : Duration -> String
durationToString duration =
    let
        seconds : Int
        seconds =
            Duration.inSeconds duration
                |> floor
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
    column [ centerX, height fill ]
        [ el [ centerX ] <|
            case model.sound of
                SoundLoading ->
                    text "Loading audio..."

                SoundError _ ->
                    text "Error loading audio 😔"

                SoundLoaded _ ->
                    Element.none
        , el [ centerX ] (text "🕘 ThirdTime")
        , el [ height (fillPortion 2) ] Element.none
        , table []
            { data = centralBlock model
            , columns =
                [ { width = shrink
                  , header = Element.none
                  , view = \( a, _, _ ) -> a
                  }
                , { width = shrink
                  , header = Element.none
                  , view = \( _, a, _ ) -> a
                  }
                , { width = shrink
                  , header = Element.none
                  , view = \( _, _, a ) -> a
                  }
                ]
            }
        , el [ height (fillPortion 1) ] Element.none
        , case model.currentState of
            Stopped ->
                el [ centerX, alignBottom ] (button Reset "Reset")

            _ ->
                Element.none
        , el [ height (fillPortion 1) ] Element.none
        , row
            [ spacing 8
            , width fill
            , alignBottom
            ]
            [ button StartWork "Work"
            , button StartBreak "Break"
            , button Stop "Stop"
            ]
        ]


centralBlock : Model -> List ( Element Msg, Element Msg, Element Msg )
centralBlock model =
    let
        firstLine : Duration -> String -> List (Element Msg) -> ( Element Msg, Element Msg, Element Msg )
        firstLine duration label other =
            ( el
                [ Font.alignRight
                , padding 8
                , Border.widthEach { left = 0, right = 0, bottom = 1, top = 0 }
                , height fill
                ]
                (text (durationToString duration))
            , column
                [ padding 8
                , spacing 8
                , Border.widthEach { left = 0, right = 0, bottom = 1, top = 0 }
                ]
                (text label :: other)
            , Element.none
            )

        line : Duration -> String -> ( Element Msg, Element Msg, Element Msg )
        line duration label =
            ( el
                [ fontSize.small
                , Font.alignRight
                , padding 8
                ]
                (text (durationToString duration))
            , el
                [ fontSize.small
                , padding 8
                ]
                (text label)
            , Element.none
            )
    in
    case model.currentState of
        Stopped ->
            [ firstLine model.workTime "Worked" [] ]

        Working { startedFrom, bankedBreakTime } ->
            let
                durationWorked : Duration
                durationWorked =
                    Duration.from startedFrom model.now
            in
            [ firstLine durationWorked
                "Worked"
                [ row
                    [ fontSize.small
                    , spacing 8
                    ]
                    [ Input.text
                        [ Background.color (Element.rgba 0 0 0 0)
                        , padding 8
                        , width (px 60)
                        ]
                        { text = model.divisor.string
                        , onChange = Divisor
                        , placeholder = Nothing
                        , label = Input.labelLeft [] (text "/")
                        }
                    , el [ moveUp 4 ] (text "=")
                    ]
                ]
            , line
                (durationWorked
                    |> Quantity.divideBy model.divisor.float
                    |> Quantity.plus bankedBreakTime
                )
                "Rest"
            , line (model.workTime |> Quantity.plus durationWorked) ("Worked of " ++ durationToString model.goal ++ " goal")
            ]

        Resting { endsOn } ->
            [ firstLine (Duration.from model.now endsOn) "Rest" []
            , line model.workTime ("Worked of " ++ durationToString model.goal ++ " goal")
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
                      , divisor = { string = "3", float = 3 }
                      , currentState = Stopped
                      , now = now
                      , sound = SoundLoading
                      , goal = Duration.minutes (4 * 60 + 13)
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
                                            Resting
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
                                            Resting
                                                { endsOn =
                                                    durationWorked
                                                        |> Quantity.divideBy model.divisor.float
                                                        |> Quantity.plus bankedBreakTime
                                                        |> Duration.addTo model.now
                                                }
                                        , workTime =
                                            model.workTime
                                                |> Quantity.plus durationWorked
                                    }

                                Resting _ ->
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

                                Resting { endsOn } ->
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

                                Resting _ ->
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

                        Divisor divisor ->
                            { model
                                | divisor =
                                    { string = divisor
                                    , float =
                                        divisor
                                            |> String.toFloat
                                            |> Maybe.withDefault model.divisor.float
                                    }
                            }
            in
            ( Just newModel, Cmd.none, Audio.cmdNone )


subscriptions : audioData -> model -> Sub Msg
subscriptions _ _ =
    Browser.Events.onAnimationFrame Tick
