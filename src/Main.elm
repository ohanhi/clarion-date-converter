module Main exposing (clarionToPosix, main, maxClarionTime, millisInDay, posixToClarion)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Iso8601
import Json.Decode as JD
import Time



--- CONVERSIONS


clarionStartDate : Time.Posix
clarionStartDate =
    -- Dec 28th 1800
    Time.millisToPosix -5333472000000


maxClarionTime : Int
maxClarionTime =
    millisInDay // 10


millisInDay : Int
millisInDay =
    86400000


type alias ClarionDate =
    { clarionDate : Int, clarionTime : Int }


posixToClarion : Time.Posix -> ClarionDate
posixToClarion time =
    let
        posix =
            Time.posixToMillis time - Time.posixToMillis clarionStartDate
    in
    { clarionDate = posix // millisInDay
    , clarionTime = (Basics.modBy millisInDay posix // 10) + 1
    }


clarionToPosix : ClarionDate -> Time.Posix
clarionToPosix { clarionDate, clarionTime } =
    let
        dateMillis =
            clarionDate * millisInDay + Time.posixToMillis clarionStartDate

        timeMillis =
            (clarionTime - 1) * 10
    in
    Time.millisToPosix (dateMillis + timeMillis)


timeToIso : Time.Posix -> String
timeToIso time =
    time
        |> Iso8601.fromTime
        |> String.replace "Z" ""



--- APPLICATION


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
    { now : Int
    , offsetMinutes : Int
    }


initModel : Flags -> ( Model, Cmd Msg )
initModel flags =
    let
        isoDate =
            (flags.now + (flags.offsetMinutes * -60000))
                |> Time.millisToPosix
                |> timeToIso
    in
    ( recalculateFromIso
        { isoDate = isoDate
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


recalculateFromClarion : Model -> Model
recalculateFromClarion model =
    let
        maybeTime =
            Maybe.map2 (\d t -> clarionToPosix { clarionDate = d, clarionTime = t })
                (String.toInt model.clarionDate)
                (String.toInt model.clarionTime)
    in
    recalculateFromIso
        { model
            | isoDate =
                maybeTime
                    |> Maybe.map (timeToIso >> String.replace "\"" "")
                    |> Maybe.withDefault ""
        }


recalculateFromHuman : Model -> Model
recalculateFromHuman model =
    recalculateFromIso
        { model | isoDate = humanToIso model |> String.replace "\"" "" }


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
        ++ "\""


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
                , clarionDate = String.fromInt clarionDate
                , clarionTime = String.fromInt clarionTime
            }

        Err _ ->
            model


view : Model -> Html Msg
view model =
    let
        field title ( minimum, maximum ) val msg =
            label []
                [ div [] [ text title ]
                , input
                    [ type_ "number"
                    , onInput msg
                    , value val
                    , Html.Attributes.min (String.fromInt minimum)
                    , Html.Attributes.max (String.fromInt maximum)
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
                [ field "Clarion Date" ( 4, 99999 ) model.clarionDate ClarionDateInput
                , field "Clarion Time" ( 1, maxClarionTime ) model.clarionTime ClarionTimeInput
                ]
            ]
        , section [ class "iso-date" ]
            [ h2 [] [ text "ISO 8601" ]
            , p []
                [ em [] [ text "Do not add a time zone, see Time zones below. Millisecond precision is ignored." ]
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
                [ field "Day" ( 1, 31 ) model.date DateInput
                , field "Month" ( 1, 12 ) model.month MonthInput
                , field "Year" ( 0, 3000 ) model.year YearInput
                , field "Hour" ( 1, 23 ) model.hour HourInput
                , field "Minute" ( 0, 59 ) model.minute MinuteInput
                , field "Second" ( 0, 59 ) model.second SecondInput
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
        the calculations will be offset by that amount – likely not what you want!
        This is why the calculated ISO 8601 date string won't have an offset either. Adjust accordingly!""" ]
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
