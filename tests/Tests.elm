module Tests exposing (dateToListTest, decoderTest, encoderTest, listToDateTest)

import Date.Extra as DE
import Expect exposing (Expectation)
import Json.Decode as JD
import Json.Encode as JE
import ListDate
import Result.Extra as RE
import Test exposing (..)


listToDateTest : Test
listToDateTest =
    describe "Convert a List Int to Date.Date"
        [ test "empty list" <|
            \_ ->
                Expect.equal Nothing (ListDate.listToDate [])
        , test "only year given" <|
            \_ ->
                Expect.equal
                    Nothing
                    (ListDate.listToDate [ 2018 ])
        , test "only year and month given" <|
            \_ ->
                Expect.equal
                    Nothing
                    (ListDate.listToDate [ 2018, 5 ])
        , test "only year, month and day given" <|
            \_ ->
                let
                    date =
                        DE.fromParts
                            2018
                            (DE.numberToMonth 5)
                            31
                            0
                            0
                            0
                            0
                in
                Expect.equal
                    (Just date)
                    (ListDate.listToDate [ 2018, 5, 31 ])
        , test "only year, month, day, and hour given" <|
            \_ ->
                let
                    date =
                        DE.fromParts
                            2018
                            (DE.numberToMonth 5)
                            31
                            15
                            0
                            0
                            0
                in
                Expect.equal
                    (Just date)
                    (ListDate.listToDate [ 2018, 5, 31, 15 ])
        , test "only year, month, day, hour, and minute given" <|
            \_ ->
                let
                    date =
                        DE.fromParts
                            2018
                            (DE.numberToMonth 5)
                            31
                            15
                            16
                            0
                            0
                in
                Expect.equal
                    (Just date)
                    (ListDate.listToDate [ 2018, 5, 31, 15, 16 ])
        , test "only year, month, day, hour, minute, and seconds given" <|
            \_ ->
                let
                    date =
                        DE.fromParts
                            2018
                            (DE.numberToMonth 5)
                            31
                            15
                            16
                            20
                            0
                in
                Expect.equal
                    (Just date)
                    (ListDate.listToDate [ 2018, 5, 31, 15, 16, 20 ])
        , test "only year, month, day, hour, minute, seconds, and miliseconds given" <|
            \_ ->
                let
                    date =
                        DE.fromParts
                            2018
                            (DE.numberToMonth 5)
                            31
                            15
                            16
                            20
                            1234
                in
                Expect.equal
                    (Just date)
                    (ListDate.listToDate [ 2018, 5, 31, 15, 16, 20, 1234 ])

        -- TODO: fuzzy test lists
        ]


dateToListTest : Test
dateToListTest =
    describe "Convert a Date to a List Int"
        [ test "with a valid date" <|
            \_ ->
                let
                    date =
                        DE.fromParts
                            2018
                            (DE.numberToMonth 5)
                            31
                            15
                            16
                            20
                            987

                    expected =
                        [ 2018, 5, 31, 15, 16, 20, 987 ]
                in
                Expect.equal
                    expected
                    (ListDate.dateToList date)

        -- TODO: fuzzy test dates
        ]


encoderTest : Test
encoderTest =
    describe "Encodes a Date as a List Int"
        [ test "with a date" <|
            \_ ->
                let
                    date =
                        DE.fromParts
                            2018
                            (DE.numberToMonth 5)
                            31
                            15
                            16
                            20
                            987

                    expected =
                        JE.list
                            [ JE.int 2018
                            , JE.int 5
                            , JE.int 31
                            , JE.int 15
                            , JE.int 16
                            , JE.int 20
                            , JE.int 987
                            ]
                in
                Expect.equal
                    expected
                    (ListDate.encoder date)
        ]


decoderTest : Test
decoderTest =
    describe "Decodes a List Int to a Date"
        [ test "with a valid date" <|
            \_ ->
                let
                    date =
                        DE.fromParts
                            2018
                            (DE.numberToMonth 5)
                            31
                            15
                            16
                            20
                            987

                    json =
                        [ 2018, 5, 31, 15, 16, 20, 987 ]
                            |> List.map JE.int
                            |> JE.list
                in
                Expect.equal
                    (Ok date)
                    (JD.decodeValue ListDate.decoder json)

        -- TODO: fuzzy test dates
        , test "with an empty list" <|
            \_ ->
                let
                    json =
                        JE.list []
                in
                Expect.true
                    "should be an Err"
                    (RE.isErr <| JD.decodeValue ListDate.decoder json)
        , test "with an only year" <|
            \_ ->
                let
                    json =
                        JE.list [ JE.int 2018 ]
                in
                Expect.true
                    "should be an Err"
                    (RE.isErr <| JD.decodeValue ListDate.decoder json)
        , test "with an only year and month" <|
            \_ ->
                let
                    json =
                        JE.list [ JE.int 2018, JE.int 2 ]
                in
                Expect.true
                    "should be an Err"
                    (RE.isErr <| JD.decodeValue ListDate.decoder json)
        ]
