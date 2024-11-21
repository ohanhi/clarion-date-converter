module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Iso8601
import Json.Decode as JD
import Time


clarionStartDate : Time.Posix
clarionStartDate =
    -- Dec 28th 1800
    Time.millisToPosix -5333472000000


millisInDay : Int
millisInDay =
    24 * 60 * 60 * 1000


type alias Model =
    { isoDate : String
    , clarionDate : String
    , clarionTime : String
    , year : String
    , month : String
    , date : String
    , hour : String
    , minute : String
    , second : String
    , milli : String
    }


type alias Flags =
    { isoDate : String
    }


initModel : Flags -> ( Model, Cmd Msg )
initModel flags =
    ( recalculateFromIso
        { isoDate = flags.isoDate
        , clarionDate = ""
        , clarionTime = ""
        , year = ""
        , month = ""
        , date = ""
        , hour = ""
        , minute = ""
        , second = ""
        , milli = ""
        }
    , Cmd.none
    )


type Msg
    = IsoDateInput String
    | ClarionDateInput String
    | ClarionTimeInput String
    | DateInput String
    | MonthInput String
    | YearInput String
    | HourInput String
    | MinuteInput String
    | SecondInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        IsoDateInput str ->
            recalculateFromIso { model | isoDate = str }

        ClarionDateInput str ->
            recalculateFromClarion { model | clarionDate = str }

        ClarionTimeInput str ->
            recalculateFromClarion { model | clarionTime = str }

        DateInput str ->
            recalculateFromHuman { model | date = str }

        MonthInput str ->
            recalculateFromHuman { model | month = str }

        YearInput str ->
            recalculateFromHuman { model | year = str }

        HourInput str ->
            recalculateFromHuman { model | hour = str }

        MinuteInput str ->
            recalculateFromHuman { model | minute = str }

        SecondInput str ->
            recalculateFromHuman { model | second = str }
    , Cmd.none
    )


type alias ParsedState =
    Maybe Time.Posix


parseModelFromClarion : Model -> ParsedState
parseModelFromClarion model =
    let
        maybeDateMillis =
            model.clarionDate
                |> String.toInt
                -- Clarion date is days elapsed from the start date
                |> Maybe.map (\num -> Time.posixToMillis clarionStartDate + num * millisInDay)

        maybeTimeMillis =
            model.clarionTime
                |> String.toInt
                -- Clarion time is 1/100ths of a second + 1
                |> Maybe.map (\num -> (num - 1) * 10)
    in
    Maybe.map2 (\dateMillis timeMillis -> Time.millisToPosix (dateMillis + timeMillis))
        maybeDateMillis
        maybeTimeMillis


recalculateFromClarion : Model -> Model
recalculateFromClarion model =
    let
        state =
            parseModelFromClarion model

        toInput accessor =
            state
                |> Maybe.map (accessor Time.utc >> String.fromInt)
                |> Maybe.withDefault ""

        monthToInput =
            state
                |> Maybe.map (Time.toMonth Time.utc >> monthToNumber >> String.fromInt)
                |> Maybe.withDefault ""
    in
    { model
        | year = toInput Time.toYear
        , month = monthToInput
        , date = toInput Time.toDay
        , hour = toInput Time.toHour
        , minute = toInput Time.toMinute
        , second = toInput Time.toSecond
        , milli = toInput Time.toMillis
        , isoDate = state |> Maybe.map (Iso8601.fromTime >> String.replace "\"" "") |> Maybe.withDefault ""
    }


recalculateFromHuman : Model -> Model
recalculateFromHuman model =
    let
        isoString =
            humanToIso model

        { clarionDate, clarionTime } =
            case JD.decodeString Iso8601.decoder isoString of
                Ok time ->
                    posixToClarion time

                Err _ ->
                    { clarionDate = "", clarionTime = "" }
    in
    { model
        | clarionDate = clarionDate
        , clarionTime = clarionTime
        , isoDate = isoString |> String.replace "\"" ""
    }


humanToIso : Model -> String
humanToIso model =
    "\""
        ++ String.padLeft 4 '0' model.year
        ++ "-"
        ++ String.padLeft 2 '0' model.month
        ++ "-"
        ++ String.padLeft 2 '0' model.date
        ++ "T"
        ++ String.padLeft 2 '0' model.hour
        ++ ":"
        ++ String.padLeft 2 '0' model.minute
        ++ ":"
        ++ String.padLeft 2 '0' model.second
        ++ "."
        ++ String.padLeft 3 '0' model.milli
        ++ "Z"
        ++ "\""


posixToClarion : Time.Posix -> { clarionDate : String, clarionTime : String }
posixToClarion time =
    let
        posix =
            Time.posixToMillis time - Time.posixToMillis clarionStartDate
    in
    { clarionDate = String.fromInt (posix // millisInDay)
    , clarionTime = String.fromInt (Basics.modBy millisInDay posix // 10 + 1)
    }


recalculateFromIso : Model -> Model
recalculateFromIso model =
    let
        parsedMillis =
            ("\"" ++ model.isoDate ++ "\"")
                |> JD.decodeString Iso8601.decoder
                |> Result.map Time.posixToMillis
    in
    case parsedMillis of
        Ok millis ->
            let
                time =
                    Time.millisToPosix millis

                toInput accessor =
                    time
                        |> accessor Time.utc
                        |> String.fromInt

                { clarionDate, clarionTime } =
                    posixToClarion time
            in
            { model
                | year = toInput Time.toYear
                , month = Time.toMonth Time.utc time |> monthToNumber |> String.fromInt
                , date = toInput Time.toDay
                , hour = toInput Time.toHour
                , minute = toInput Time.toMinute
                , second = toInput Time.toSecond
                , milli = toInput Time.toMillis
                , clarionDate = clarionDate
                , clarionTime = clarionTime
            }

        Err _ ->
            { model
                | year = ""
                , month = ""
                , date = ""
                , hour = ""
                , minute = ""
                , second = ""
                , milli = ""
            }


view : Model -> Html Msg
view model =
    let
        field title val msg =
            label []
                [ div [] [ text title ]
                , input
                    [ type_ "number"
                    , onInput msg
                    , value val
                    , size 5
                    ]
                    []
                ]
    in
    main_
        []
        [ header [] [ h1 [] [ text "Clarion Date Time Converter" ] ]
        , section [ class "clarion-date" ]
            [ h2 [] [ text "Clarion" ]
            , div [ class "clarion-fields" ]
                [ field "Clarion Date" model.clarionDate ClarionDateInput
                , field "Clarion Time" model.clarionTime ClarionTimeInput
                ]
            ]
        , section [ class "iso-date" ]
            [ h2 [] [ text "ISO 8601" ]
            , p []
                [ em [] [ text "Time zone should be set to 'Z' no matter what offset you are using, see Time zones below." ]
                ]
            , label []
                [ div [] [ text "ISO Date String" ]
                , input
                    [ type_ "text"
                    , onInput IsoDateInput
                    , value model.isoDate
                    ]
                    []
                ]
            ]
        , section [ class "human-date" ]
            [ h2 [] [ text "Human" ]
            , div [ class "human-fields" ]
                [ field "Day" model.date DateInput
                , field "Month" model.month MonthInput
                , field "Year" model.year YearInput
                , field "Hour" model.hour HourInput
                , field "Minute" model.minute MinuteInput
                , field "Second" model.second SecondInput
                ]
            ]
        , section [ class "info" ]
            [ h2 [] [ text "Information on Clarion date and time" ]
            , p [] [ text """Clarion date is the number of days since 28th of December, 1800.
        Interestingly, the first valid date is 1st of January, 1801.
        This means the valid values range from 4 upwards.""" ]
            , p [] [ text """Clarion time is centiseconds from midnight, indexed from 1.
        That is, 1/100ths of a second from midnight + 1.
        There are 86400 seconds in a day, which means the valid values range from 1 to 864000""" ]
            , h3 [ id "time-zones" ] [ text "Time zones" ]
            , p [] [ text """Clarion date time is “local”, which means it does not
        encode time zones in any way. If you input an ISO 8601 string with a time zone offset,
        the calculations will be offset by that amount.
        This is why the ISO 8601 date string always defaults to 'Z',
        i.e. the UTC standard time zone. This may well be incorrect for your""" ]
            ]
        , footer []
            [ p []
                [ text "This converter is Open Source under BSD-3. "
                , a [ href "https://github.com/ohanhi/clarion-date-converter" ] [ text "GitHub" ]
                ]
            ]
        ]


monthToNumber : Time.Month -> number
monthToNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


main : Program Flags Model Msg
main =
    Browser.element
        { init = initModel
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
