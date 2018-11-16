module Util exposing (clockDisplay, findStringIn, getListInPairs)

import Time exposing (Weekday(..))



-- find if any strings in a given list are contained within a longer string


findStringIn : List String -> String -> String
findStringIn strings stringToSearchIn =
    case List.head strings of
        Just string ->
            if String.contains string stringToSearchIn then
                string

            else
                findStringIn (List.drop 1 strings) stringToSearchIn

        Nothing ->
            ""



-- convert a list into a list of lists of length 2. Used to pair day and night weather entries chronologically.


getListInPairs : List a -> List (List a) -> List (List a)
getListInPairs originalList pairedList =
    case List.length originalList of
        0 ->
            List.reverse pairedList

        1 ->
            List.reverse pairedList

        _ ->
            getListInPairs (List.drop 2 originalList) (List.take 2 originalList :: pairedList)



-- build the text of a basic digital clock display in the format "[DAY_OF_WEEK] [HR]:[MIN][MIN] [AM/PM]"


clockDisplay : Time.Zone -> Time.Posix -> String
clockDisplay timezone time =
    let
        hour =
            Time.toHour timezone time

        minute =
            String.padLeft 2 '0' <|
                String.fromInt <|
                    Time.toMinute timezone time

        weekday =
            case Time.toWeekday timezone time of
                Mon ->
                    "Monday"

                Tue ->
                    "Tuesday"

                Wed ->
                    "Wednesday"

                Thu ->
                    "Thursday"

                Fri ->
                    "Friday"

                Sat ->
                    "Saturday"

                Sun ->
                    "Sunday"
    in
    if hour > 12 then
        weekday ++ " " ++ String.fromInt (hour - 12) ++ ":" ++ minute ++ " PM"

    else if hour == 0 then
        weekday ++ " " ++ "12:" ++ minute ++ " AM"

    else
        weekday ++ " " ++ String.fromInt hour ++ ":" ++ minute ++ " AM"
