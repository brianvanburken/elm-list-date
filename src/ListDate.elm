module ListDate exposing (dateToList, decoder, encoder, listToDate)

{-| Sometimes an API returns a List of Int's and you have to deal with it. This
package helps with the conversion from and to.


# Conversion

@docs dateToList, listToDate


# JSON

@docs decoder, encoder

-}

import Date exposing (Date)
import Date.Extra as DE exposing (Interval(..))
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Extra as JDE
import Json.Encode as JE


{-| Provides a decoder that will convert a List of Int to a Date.
-}
decoder : Decoder Date
decoder =
    JD.list JD.int
        |> JD.andThen
            (listToDate >> JDE.fromResult)


{-| Provides an encoder that will convert a Date to a List of Int's.
-}
encoder : Date -> JE.Value
encoder =
    dateToList
        >> List.map JE.int
        >> JE.list


{-| Converts a List of Int's to a Maybe Date. It expects a total of 7 integers
representing year till milisceonds. If this is not the case it will fill in the
rest with zero's.

    listToDate [] -- Err ..
    listToDate [ 2018 ] -- Err ..
    listToDate [ 2018, 5 ] -- Err ..
    listToDate [ 2018, 5, 31 ] -- Ok <Thu May 31 00:00:00 GMT+0000>
    listToDate [ 2018, 5, 31, 15 ] -- Ok <Thu May 31 15:16:00 CMT+0000>
    listToDate [ 2018, 5, 31, 15, 16 ] -- Ok <Thu May 31 15:16:00 CMT+0000>
    listToDate [ 2018, 5, 31, 15, 16, 20, 987 ] -- Ok <Thu May 31 15:16:20 CMT+0000>

-}
listToDate : List Int -> Result String Date
listToDate list =
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
                DE.fromParts
                    year
                    (DE.numberToMonth month)
                    day
                    hours
                    minutes
                    seconds
                    millis

        _ ->
            Err "Invalid list of data given for date."


ensureSize : Int -> List Int -> List Int
ensureSize size list =
    List.take size (list ++ List.repeat size 0)


{-| Converts a date to a List of Int's. With the head being the year and the last
the milliseconds.

    dateToList date -- [ 2018, 5, 31,  0,  0,  0, 0 ]
    dateToList date -- [ 2018, 5, 31, 15, 16,  0, 0 ]
    dateToList date -- [ 2018, 5, 31, 15, 16, 20, 1000 ]

-}
dateToList : Date -> List Int
dateToList date =
    -- Date
    [ Date.year date
    , DE.monthNumber date
    , Date.day date

    -- Time
    , Date.hour date
    , Date.minute date
    , Date.second date
    , Date.millisecond date
    ]
