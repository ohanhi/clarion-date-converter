module TestMain exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (clarionToPosix, maxClarionTime, posixToClarion)
import Test exposing (..)
import Time


maxSafeInteger : Int
maxSafeInteger =
    2 ^ 31 - 1


firstOf1801 : Time.Posix
firstOf1801 =
    Time.millisToPosix -5333126400000


epoch : Time.Posix
epoch =
    Time.millisToPosix 0


suite : Test
suite =
    describe "Datetime conversions"
        [ describe "Posix to Clarion"
            [ test "First Clarion date" <|
                \_ ->
                    Expect.equal
                        { clarionDate = 4, clarionTime = 1 }
                        (posixToClarion firstOf1801)
            , test "Posix epoch" <|
                \_ ->
                    Expect.equal
                        { clarionDate = 61730, clarionTime = 1 }
                        (posixToClarion epoch)
            , test "Posix epoch + 10 hours" <|
                \_ ->
                    let
                        epochPlus10h =
                            Time.millisToPosix (10 * 60 * 60 * 1000)
                    in
                    Expect.equal
                        { clarionDate = 61730, clarionTime = 3600001 }
                        (posixToClarion epochPlus10h)
            ]
        , describe "Clarion to Posix"
            [ test "First Clarion date" <|
                \_ ->
                    Expect.equal firstOf1801
                        (clarionToPosix { clarionDate = 4, clarionTime = 1 })
            , test "Posix epoch" <|
                \_ ->
                    Expect.equal
                        epoch
                        (clarionToPosix { clarionDate = 61730, clarionTime = 1 })
            , test "Late in the day" <|
                \_ ->
                    Expect.equal
                        (Time.millisToPosix ((23 * 3600 + 59 * 60 + 59) * 1000))
                        (clarionToPosix { clarionDate = 61730, clarionTime = 8639901 })
            ]
        , describe "Clarion -> Posix -> Clarion"
            [ fuzz2
                (Fuzz.intRange 4 (maxSafeInteger // maxClarionTime))
                (Fuzz.intRange 1 maxClarionTime)
                "Any Clarion values"
              <|
                \date time ->
                    let
                        clarion =
                            { clarionDate = date, clarionTime = time }
                    in
                    clarion
                        |> clarionToPosix
                        |> posixToClarion
                        |> Expect.equal clarion
            ]
        , describe "Posix -> Clarion -> Posix"
            [ fuzz
                (Fuzz.intRange -(maxSafeInteger // 10) (maxSafeInteger // 10))
                "Any Posix values (ignoring millisecond places)"
              <|
                \millis ->
                    let
                        time =
                            Time.millisToPosix (millis * 10)
                    in
                    time
                        |> posixToClarion
                        |> clarionToPosix
                        |> Expect.equal time
            ]
        ]
