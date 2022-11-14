module DateRange exposing
    ( DateRange
    , contains
    , days
    , fromDates
    , fromList
    , fromMaybes
    , setEnd
    , setStart
    , toDates
    , toEnd
    , toList
    , toStart
    )

import Date exposing (Date, Unit(..))
import List.Extra as List


type DateRange
    = DateRange InternalRange


type alias InternalRange =
    { start : Date
    , end : Date
    }


fromDates : Date -> Date -> DateRange
fromDates start end =
    DateRange <|
        if Date.diff Date.Days start end >= 0 then
            InternalRange start end

        else
            InternalRange end start


fromMaybes : Maybe Date -> Maybe Date -> Maybe DateRange
fromMaybes maybeStart maybeEnd =
    case ( maybeStart, maybeEnd ) of
        ( Just s, Just e ) ->
            fromDates s e |> Just

        ( Just s, Nothing ) ->
            fromDates s s |> Just

        ( Nothing, Just e ) ->
            fromDates e e |> Just

        _ ->
            Nothing


setStart : Date -> DateRange -> DateRange
setStart start (DateRange { end }) =
    fromDates start end


setEnd : Date -> DateRange -> DateRange
setEnd end (DateRange { start }) =
    fromDates start end


toStart : DateRange -> Date
toStart (DateRange { start }) =
    start


toEnd : DateRange -> Date
toEnd (DateRange { end }) =
    end


toDates : DateRange -> ( Date, Date )
toDates (DateRange { start, end }) =
    ( start, end )


toList : DateRange -> List Date
toList (DateRange { start, end }) =
    Date.range Date.Day 1 start (Date.add Date.Days 1 end)


days : DateRange -> Int
days (DateRange { start, end }) =
    Date.diff Days start end + 1


contains : Date -> DateRange -> Bool
contains date (DateRange { start, end }) =
    Date.isBetween start end date


fromList : List Date -> List DateRange
fromList dates =
    let
        sortedDays =
            List.uniqueBy Date.toIsoString dates
                |> List.sortBy Date.toIsoString

        makeRange d ranges =
            case List.head ranges of
                Just r ->
                    if Date.diff Date.Days (toStart r) d == -1 then
                        fromDates d (toEnd r) :: List.drop 1 ranges

                    else
                        fromDates d d
                            :: ranges

                Nothing ->
                    [ fromDates d d ]
    in
    List.foldr makeRange [] sortedDays
