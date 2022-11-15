module DateRangeTests exposing (isWithinTests)

import Date
import DateRange
import Expect
import Test exposing (Test)


isWithinTests : Test
isWithinTests =
    Test.describe "Date range tests"
        [ Test.test "Date one day before start is not within" <|
            \_ ->
                DateRange.fromDates (Date.fromOrdinalDate 2000 2) (Date.fromOrdinalDate 2000 3)
                    |> DateRange.contains (Date.fromOrdinalDate 2000 1)
                    |> Expect.equal False
        , Test.test "Date on start date is within" <|
            \_ ->
                DateRange.fromDates (Date.fromOrdinalDate 2000 2) (Date.fromOrdinalDate 2000 3)
                    |> DateRange.contains (Date.fromOrdinalDate 2000 2)
                    |> Expect.equal True
        , Test.test "Date on end date is within" <|
            \_ ->
                DateRange.fromDates (Date.fromOrdinalDate 2000 2) (Date.fromOrdinalDate 2000 3)
                    |> DateRange.contains (Date.fromOrdinalDate 2000 3)
                    |> Expect.equal True
        , Test.test "Date one day after end date is not within" <|
            \_ ->
                DateRange.fromDates (Date.fromOrdinalDate 2000 2) (Date.fromOrdinalDate 2000 3)
                    |> DateRange.contains (Date.fromOrdinalDate 2000 4)
                    |> Expect.equal False
        ]
