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
                Expect.equal
                    (Err "Invalid list of data given for date. Need at least year, month, and day.")
                    (ListDate.listToDate utc [])
        , test "only year given" <|
            \_ ->
                Expect.equal
                    (Err "Invalid list of data given for date. I've gotten the year, but still need month and day.")
                    (ListDate.listToDate utc [ 2018 ])
        , test "only year and month given" <|
            \_ ->
                Expect.equal
                    (Err "Invalid list of data given for date. I've gotten the year and month, but still need the day.")
                    (ListDate.listToDate utc [ 2018, 5 ])
        , test "only year, month and day given" <|
            \_ ->
                let
                    posix =
                        Time.millisToPosix 1527724800000
                in
                Expect.equal
                    (Ok posix)
                    (ListDate.listToDate utc [ 2018, 5, 31 ])
        , test "only year, month, day, and hour given" <|
            \_ ->
                let
                    posix =
                        Time.millisToPosix 1527778800000
                in
                Expect.equal
                    (Ok posix)
                    (ListDate.listToDate utc [ 2018, 5, 31, 15 ])
        , test "only year, month, day, hour, and minute given" <|
            \_ ->
                let
                    posix =
                        Time.millisToPosix 1527779760000
                in
                Expect.equal
                    (Ok posix)
                    (ListDate.listToDate utc [ 2018, 5, 31, 15, 16 ])
        , test "only year, month, day, hour, minute, and seconds given" <|
            \_ ->
                let
                    posix =
                        Time.millisToPosix 1527779780000
                in
                Expect.equal
                    (Ok posix)
                    (ListDate.listToDate utc [ 2018, 5, 31, 15, 16, 20 ])
        , test "only year, month, day, hour, minute, seconds, and miliseconds given" <|
            \_ ->
                let
                    posix =
                        Time.millisToPosix 1527779780987
                in
                Expect.equal
                    (Ok posix)
                    (ListDate.listToDate utc [ 2018, 5, 31, 15, 16, 20, 987 ])
        ]


dateToListTest : Test
dateToListTest =
    describe "Convert a Date to a List Int"
        [ test "with a valid date" <|
            \_ ->
                let
                    posix =
                        Time.millisToPosix 1527779780987

                    expected =
                        [ 2018, 5, 31, 15, 16, 20, 987 ]
                in
                Expect.equal
                    expected
                    (ListDate.dateToList utc posix)
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
                Expect.equal
                    (Ok posix)
                    (JD.decodeValue (ListDate.decoder utc) json)
        , test "with an empty list" <|
            \_ ->
                let
                    json =
                        JE.list JE.int []
                in
                Expect.true
                    "should be an Err"
                    (RE.isErr <| JD.decodeValue (ListDate.decoder utc) json)
        , test "with an only year" <|
            \_ ->
                let
                    json =
                        JE.list JE.int [ 2018 ]
                in
                Expect.true
                    "should be an Err"
                    (RE.isErr <| JD.decodeValue (ListDate.decoder utc) json)
        , test "with an only year and month" <|
            \_ ->
                let
                    json =
                        JE.list JE.int [ 2018, 2 ]
                in
                Expect.true
                    "should be an Err"
                    (RE.isErr <| JD.decodeValue (ListDate.decoder utc) json)
        ]
