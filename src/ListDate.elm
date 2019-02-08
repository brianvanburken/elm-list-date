module ListDate exposing
    ( dateToList, listToDate
    , decoder, encoder
    )

{-| Sometimes an API returns a List of Int's and you have to deal with it. This
package helps with the conversion from and to.


# Conversion

@docs dateToList, listToDate


# JSON

@docs decoder, encoder

-}

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JDE
import Json.Encode as JE
import Time exposing (Month(..), Posix, Zone)
import Time.Extra as TE


{-| Provides a decoder that will convert a List of Int to a Date.
-}
decoder : Zone -> Decoder Posix
decoder zone =
    JD.list JD.int
        |> JD.andThen
            (listToDate zone >> JDE.fromResult)


{-| Provides an encoder that will convert a Date to a List of Int's.
-}
encoder : Zone -> Posix -> JE.Value
encoder zone =
    dateToList zone
        >> JE.list JE.int


{-| Converts a List of Int's to a Maybe Date. It expects a total of 7 integers
representing year till milisceonds. If this is not the case it will fill in the
rest with zero's.

    listToDate Time.utc [] -- Err ..

    listToDate Time.utc [ 2018 ] -- Err ..

    listToDate Time.utc [ 2018, 5 ] -- Err ..

    listToDate Time.utc [ 2018, 5, 31 ] -- Ok (Posix 1527724800000)

    listToDate Time.utc [ 2018, 5, 31, 15 ] -- Ok (Posix 1527778800000)

    listToDate Time.utc [ 2018, 5, 31, 15, 16 ] -- Ok (Posix 1527779760000)

    listToDate Time.utc [ 2018, 5, 31, 15, 16, 20, 987 ] -- Ok (Posix 1527779780987)

-}
listToDate : Zone -> List Int -> Result String Posix
listToDate zone list =
    let
        l =
            ensureSize 7 list
    in
    case l of
        [ 0, 0, 0, 0, 0, 0, 0 ] ->
            Err "Invalid list of data given for date. Need at least year, month, and day."

        [ _, 0, 0, 0, 0, 0, 0 ] ->
            Err "Invalid list of data given for date. I've gotten the year, but still need month and day."

        [ _, _, 0, 0, 0, 0, 0 ] ->
            Err "Invalid list of data given for date. I've gotten the year and month, but still need the day."

        [ year, month, day, hours, minutes, seconds, millis ] ->
            Ok <|
                TE.partsToPosix zone
                    { year = year
                    , month = intToMonth month
                    , day = day
                    , hour = hours
                    , minute = minutes
                    , second = seconds
                    , millisecond = millis
                    }

        _ ->
            Err "Invalid list of data given for date."


ensureSize : Int -> List Int -> List Int
ensureSize size list =
    List.take size (list ++ List.repeat size 0)


{-| Converts a date to a List of Int's. With the head being the year and the last
the milliseconds.

    dateToList Time.utc (Posix 1527724800000) -- [ 2018, 5, 31,  0,  0,  0, 0 ]

    dateToList Time.utc (Posix 1527779760000) -- [ 2018, 5, 31, 15, 16,  0, 0 ]

    dateToList Time.utc (Posix 1527779780987) -- [ 2018, 5, 31, 15, 16, 20, 987 ]

-}
dateToList : Zone -> Posix -> List Int
dateToList zone timestamp =
    -- Date
    [ Time.toYear zone timestamp
    , Time.toMonth zone timestamp |> monthToInt
    , Time.toDay zone timestamp

    -- Time
    , Time.toHour zone timestamp
    , Time.toMinute zone timestamp
    , Time.toSecond zone timestamp
    , Time.toMillis zone timestamp
    ]


intToMonth : Int -> Month
intToMonth month =
    case month of
        2 ->
            Feb

        3 ->
            Mar

        4 ->
            Apr

        5 ->
            May

        6 ->
            Jun

        7 ->
            Jul

        8 ->
            Aug

        9 ->
            Sep

        10 ->
            Oct

        11 ->
            Nov

        12 ->
            Dec

        _ ->
            Jan


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12
