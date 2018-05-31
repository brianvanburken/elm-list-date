module Tests exposing (dateToListTest, decoderTest, encoderTest, listToDateTest)

-- import Fuzz exposing (Fuzzer, int, list, string)

import Date.Extra as DE
import Expect exposing (Expectation)
import ListDate
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
        ]


dateToListTest : Test
dateToListTest =
    todo "Implement dateToListTest"


encoderTest : Test
encoderTest =
    todo "Implement encoderTest"


decoderTest : Test
decoderTest =
    todo "Implemdet decoderTest"
