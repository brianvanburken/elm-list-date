module ListDate exposing (dateToList, decoder, encoder, listToDate)

{-| Sometimes an API returns a List of Int's and you have to deal with it. This
package helps with the conversion from and to.


# Conversion

@docs dateToList, listToDate


# JSON

@docs decoder, encoder

-}

import Date
import Date.Extra as DE exposing (Interval(..))
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE


{-| Provides a decoder that will convert a List of Int to a Date.
-}
decoder : Decoder Date.Date
decoder =
    JD.list JD.int
        |> JD.andThen
            (listToDate
                >> Maybe.map JD.succeed
                >> Maybe.withDefault (JD.fail "Unable to parse date")
            )


{-| Provides an encoder that will convert a Date to a List of Int's.
-}
encoder : Date.Date -> JE.Value
encoder =
    dateToList
        >> List.map JE.int
        >> JE.list


{-| Converts a List of Int's to a Maybe Date. It expects a total of 7 integers
representing year till milisceonds. If this is not the case it will fill in the
rest with zero's.

    listToDate [] -- Nothing
    listToDate [ 2018, 5, 31 ] -- Just <Thu May 31 00:00:00 GMT+0000>
    listToDate [ 2018, 5, 31, 15, 16 ] -- Just <Thu May 31 15:16:00 CMT+0000>
    listToDate [ 2018, 5, 31, 15, 16, 20, 1000 ] -- Just <Thu May 31 15:16:20 CMT+0000>
    listToDate [ 2018, 2, 31, 15, 16, 20, 1000 ] -- Nothing

-}
listToDate : List Int -> Maybe Date.Date
listToDate list =
    let
        l =
            ensureSize 7 list
    in
    case l of
        [ 0, 0, 0, 0, 0, 0, 0 ] ->
            Nothing

        [ _, 0, 0, 0, 0, 0, 0 ] ->
            Nothing

        [ _, _, 0, 0, 0, 0, 0 ] ->
            Nothing

        [ year, month, day, hours, minutes, seconds, millis ] ->
            Just <|
                DE.fromParts
                    year
                    (DE.numberToMonth month)
                    day
                    hours
                    minutes
                    seconds
                    millis

        _ ->
            Nothing


ensureSize : Int -> List Int -> List Int
ensureSize size list =
    List.take size (list ++ List.repeat size 0)


{-| Converts a date to a List of Int's. With the head being the year and the last
the milliseconds.

    dateToList date -- [ 2018, 5, 31,  0,  0,  0, 0 ]
    dateToList date -- [ 2018, 5, 31, 15, 16,  0, 0 ]
    dateToList date -- [ 2018, 5, 31, 15, 16, 20, 1000 ]

-}
dateToList : Date.Date -> List Int
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
