module Tests exposing (dateToListTest, decoderTest, encoderTest, listToDateTest)

import Expect exposing (Expectation)
import Json.Decode as JD
import Json.Encode as JE
import ListDate
import Result.Extra as RE
import Test exposing (..)
import Time exposing (Posix, utc)


listToDateTest : Test
listToDateTest =
    describe "Convert a List Int to Date.Date"
        [ test "empty list" <|
            \_ ->
                ListDate.listToDate utc []
                    |> Expect.equal
                        (Err "Invalid list of data given for date. Need at least year, month, and day.")
        , test "only year given" <|
            \_ ->
                ListDate.listToDate utc [ 2018 ]
                    |> Expect.equal
                        (Err "Invalid list of data given for date. I've gotten the year, but still need month and day.")
        , test "only year and month given" <|
            \_ ->
                ListDate.listToDate utc [ 2018, 5 ]
                    |> Expect.equal
                        (Err "Invalid list of data given for date. I've gotten the year and month, but still need the day.")
        , test "only year, month and day given" <|
            \_ ->
                Time.millisToPosix 1527724800000
                    |> Ok
                    |> Expect.equal (ListDate.listToDate utc [ 2018, 5, 31 ])
        , test "only year, month, day, and hour given" <|
            \_ ->
                Time.millisToPosix 1527778800000
                    |> Ok
                    |> Expect.equal
                        (ListDate.listToDate utc [ 2018, 5, 31, 15 ])
        , test "only year, month, day, hour, and minute given" <|
            \_ ->
                Time.millisToPosix 1527779760000
                    |> Ok
                    |> Expect.equal
                        (ListDate.listToDate utc [ 2018, 5, 31, 15, 16 ])
        , test "only year, month, day, hour, minute, and seconds given" <|
            \_ ->
                Time.millisToPosix 1527779780000
                    |> Ok
                    |> Expect.equal
                        (ListDate.listToDate utc [ 2018, 5, 31, 15, 16, 20 ])
        , test "only year, month, day, hour, minute, seconds, and milliseconds given" <|
            \_ ->
                Time.millisToPosix 1527779780987
                    |> Ok
                    |> Expect.equal
                        (ListDate.listToDate utc [ 2018, 5, 31, 15, 16, 20, 987 ])
        ]


dateToListTest : Test
dateToListTest =
    describe "Convert a Date to a List Int"
        [ test "with a valid date" <|
            \_ ->
                Time.millisToPosix 1527779780987
                    |> ListDate.dateToList utc
                    |> Expect.equal
                        [ 2018, 5, 31, 15, 16, 20, 987 ]
        ]


encoderTest : Test
encoderTest =
    describe "Encodes a Date as a List Int"
        [ test "with a date" <|
            \_ ->
                let
                    posix =
                        Time.millisToPosix 1527779780987

                    expected =
                        JE.list JE.int
                            [ 2018, 5, 31, 15, 16, 20, 987 ]
                in
                Expect.equal
                    expected
                    (ListDate.encoder utc posix)
        ]


decoderTest : Test
decoderTest =
    describe "Decodes a List Int to a Date"
        [ test "with a valid date" <|
            \_ ->
                let
                    posix =
                        Time.millisToPosix 1527779780987

                    json =
                        [ 2018, 5, 31, 15, 16, 20, 987 ]
                            |> JE.list JE.int
                in
                Expect.equal (Ok posix)
                    (JD.decodeValue (ListDate.decoder utc) json)
        , test "with an empty list" <|
            \_ ->
                let
                    json =
                        JE.list JE.int []
                in
                json
                    |> JD.decodeValue (ListDate.decoder utc)
                    |> RE.isErr
                    |> Expect.equal True
                    |> Expect.onFail "expected to be an Err"
        , test "with an only year" <|
            \_ ->
                let
                    json =
                        JE.list JE.int [ 2018 ]
                in
                json
                    |> JD.decodeValue (ListDate.decoder utc)
                    |> RE.isErr
                    |> Expect.equal True
                    |> Expect.onFail "expected to be an Err"
        , test "with an only year and month" <|
            \_ ->
                let
                    json =
                        JE.list JE.int [ 2018, 2 ]
                in
                json
                    |> JD.decodeValue (ListDate.decoder utc)
                    |> RE.isErr
                    |> Expect.equal True
                    |> Expect.onFail "expected to be an Err"
        ]
