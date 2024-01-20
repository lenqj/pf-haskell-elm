module Model.Interval exposing (Interval, compare, full, length, oneYear, open, view, withDurationMonths, withDurationYears)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Model.Date as Date exposing (Date, Month(..))
import Model.Util exposing (chainCompare)
import Debug exposing(toString)
type Interval
    = Interval { start : Date, end : Maybe Date }


{-| Create an `Interval` from 2 `Date`s. If the second date is before the first the date, the function will return
`Nothing`. When possible, use the `withDurationMonths` or `withDurationYears` functions.
-}
full : Date -> Date -> Maybe Interval
full start end =
    if Date.compare start end == GT then
        Nothing

    else
        Just <| Interval { start = start, end = Just end }


{-| Create an `Interval` from a start year, start month, and a duration in months.
The start year and month are explicitly required because the duration in months is only specified if the start date
also includes a month.
This function, (assuming positive inputs) by definition, can always return a valid `Interval`.
-}
withDurationMonths : Int -> Month -> Int -> Interval
withDurationMonths startYear startMonth duration =
    let
        start =
            Date.full startYear startMonth

        end =
            Date.offsetMonths duration start
    in
    Interval { start = start, end = Just end }


{-| Create an `Interval` from a start `Date`, and a duration in years. This function, (assuming positive inputs)
by definition, can always return a valid `Interval`.
-}
withDurationYears : Date -> Int -> Interval
withDurationYears start duration =
    let
        end =
            Date.offsetMonths (duration * 12) start
    in
    Interval { start = start, end = Just end }


{-| Create an open `Interval` from a start `Date`. Usually used for creating ongoing events.
-}
open : Date -> Interval
open start =
    Interval { start = start, end = Nothing }


{-| Convenience function to create an `Interval` that represents one year.
-}
oneYear : Int -> Interval
oneYear year =
    withDurationYears (Date.onlyYear year) 1


{-| The length of an Interval, in (years, months)
-}
length : Interval -> Maybe ( Int, Int )
length (Interval interval) =
    interval.end
        |> Maybe.andThen (Date.monthsBetween interval.start)
        |> Maybe.map (\totalMonths -> ( totalMonths // 12, modBy 12 totalMonths ))


{-| Compares two intervals.

Intervals are first compared compare by the `start` field.
If the `start` field is equal, the they are compare by the `end` fields:

  - If both are missing (`Nothing`), the intervals are considered equal
  - If both are present (`Just`), the longer interval is considered greater
  - If only one interval is open (its `end` field is `Nothing`) then it will be considered greater

```
    import Model.Date as Date

    Model.Interval.compare (oneYear 2019) (oneYear 2020) --> LT
    Model.Interval.compare (oneYear 2019) (withDurationYears (Date.onlyYear 2020) 2) --> LT
    Model.Interval.compare (withDurationMonths 2019 Date.Jan 2) (withDurationMonths 2019 Date.Jan 2) --> EQ
    Model.Interval.compare (withDurationMonths 2019 Date.Feb 2) (withDurationMonths 2019 Date.Jan 2) --> GT
    Model.Interval.compare (withDurationMonths 2019 Date.Jan 2) (open (Date.onlyYear 2019)) --> LT
```

-}
compare : Interval -> Interval -> Order
compare (Interval intA) (Interval intB) =
    chainCompare 
    (
        case (intA.end, intB.end) of
            (Just int1, Just int2) -> Date.compare int1 int2
            (Just _, Nothing) -> LT
            (Nothing, Just _) -> GT
            (Nothing, Nothing) -> EQ
    ) 
    (
        Date.compare intA.start intB.start
    )
    --Debug.todo "Implement Model.Interval.compare"


view : Interval -> Html msg
view interval =
    let 
        (Interval int) = interval
    in
    div [ class "interval" ] 
    [
        p [ class "interval-start" ] [ Date.view int.start],
        p [ class "interval-end" ] 
        [ 
            case int.end of
                Just tempEnd -> Date.view tempEnd
                Nothing -> div [] [ p [] [ text " Present " ] ]
        ],
        case length interval of
            Just lenghtInt -> p [ class "interval-length" ] [ text (toString(lenghtInt))]
            Nothing -> p [ class "interval-length" ] []
    ]

    --Debug.todo "Implement Model.Interval.view"
