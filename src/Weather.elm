module Weather exposing (Weather, getWeatherForDay, weatherConditionIcon, weatherToday)

import Html exposing (Html, div, li, p, span, strong, text)
import Html.Attributes exposing (class)
import List.Extra exposing (getAt)
import SvgAssets exposing (svgWeatherCloudy, svgWeatherNone, svgWeatherRain, svgWeatherSleet, svgWeatherSnow, svgWeatherSunny)
import Time exposing (Weekday(..))
import Util exposing (findStringIn)


type alias Weather =
    { temperature : Int, unitForTemp : String, condition : String, time : Time.Posix }


weatherToday : List Weather -> Weather
weatherToday weekWeather =
    case List.head weekWeather of
        Just first ->
            first

        Nothing ->
            Weather 0 "-" "-" (Time.millisToPosix 0)


getWeatherForDay : Time.Zone -> List Weather -> Html a
getWeatherForDay timezone day =
    let
        we1 =
            case getAt 0 day of
                Just first ->
                    first

                Nothing ->
                    Weather 0 "-" "-" (Time.millisToPosix 0)

        we2 =
            case getAt 1 day of
                Just second ->
                    second

                Nothing ->
                    Weather 0 "-" "-" (Time.millisToPosix 0)

        weekday =
            case Time.toWeekday timezone we1.time of
                Mon ->
                    "Mon"

                Tue ->
                    "Tue"

                Wed ->
                    "Wed"

                Thu ->
                    "Thu"

                Fri ->
                    "Fri"

                Sat ->
                    "Sat"

                Sun ->
                    "Sun"
    in
    li [ class "weather-forecast__period" ]
        [ p [ class "weather-forecast__period__name" ] [ text weekday ]
        , div [ class "weather-icon weather-forecast__period__icon" ] [ weatherConditionIcon we2 ]
        , p []
            [ span [ class "weather-forecast__period__temperature weather-forecast__period__temperature--high" ]
                [ strong []
                    [ text (String.fromInt we1.temperature ++ "°" ++ we1.unitForTemp)
                    ]
                ]
            , span [ class "weather-forecast__period__temperature weather-forecast__period__temperature--low" ]
                [ text (String.fromInt we2.temperature ++ "°" ++ we2.unitForTemp)
                ]
            ]
        ]


weatherConditionIcon : Weather -> Html msg
weatherConditionIcon weather =
    let
        weatherConditions =
            [ "mostly clear", "cloudy", "snow", "sleet", "rain", "sunny", "clear" ]
    in
    case findStringIn weatherConditions (String.toLower weather.condition) of
        "mostly clear" ->
            svgWeatherSunny

        "cloudy" ->
            svgWeatherCloudy

        "snow" ->
            svgWeatherSnow

        "sleet" ->
            svgWeatherSleet

        "rain" ->
            svgWeatherRain

        "sunny" ->
            svgWeatherSunny

        "clear" ->
            svgWeatherSunny

        _ ->
            svgWeatherCloudy
