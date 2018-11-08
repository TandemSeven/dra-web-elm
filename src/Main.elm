module Main exposing (main)

import Browser
import Html exposing (Html, aside, button, div, header, img, input, li, main_, p, section, span, strong, sup, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Svg exposing (g, path, svg)
import Svg.Attributes exposing (d, viewBox)



-- App Architecture


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Weather =
    { temperature : Int, unitForTemp : String, condition : String }


type alias Locality =
    { zip : String, city : String, region : String, photo : String, lat : String, lng : String }


type alias Model =
    { zipInput : String, locality : Locality, weather : List Weather }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "00000" (Locality "00000" "" "" "" "" "") [], fetchLocationFromIP )



-- UPDATE


type Msg
    = FetchLocationFromZip String
    | FetchLocationFromIP
    | FetchLocationImage ( String, String )
    | FetchWeather ( String, String )
    | ReceivedLocationFromZip (Result Http.Error LocationFromZip)
    | ReceivedLocationFromIP (Result Http.Error LocationFromIP)
    | ReceivedLocationImage (Result Http.Error Images)
    | ReceivedWeather (Result Http.Error (List Weather))
    | ChangeZipInput String


setLocalityFromZip : LocationFromZip -> Locality -> Locality
setLocalityFromZip location locality =
    { locality | zip = location.zip, city = location.place.city, lat = location.place.latitude, lng = location.place.longitude }


setLocalityFromIP : LocationFromIP -> Locality -> Locality
setLocalityFromIP location locality =
    { locality | zip = location.zip, city = location.city, region = location.region, lat = String.fromFloat location.latitude, lng = String.fromFloat location.longitude }


setPhoto : Images -> Locality -> Locality
setPhoto newImages locality =
    { locality | photo = newImages.web }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchLocationFromZip url ->
            ( model, fetchLocationFromZip url )

        FetchLocationFromIP ->
            ( model, fetchLocationFromIP )

        FetchLocationImage ( lat, lng ) ->
            ( model, fetchLocationImage ( lat, lng ) )

        FetchWeather ( lat, lng ) ->
            ( model, fetchWeeklyWeather ( lat, lng ) )

        ReceivedLocationFromZip result ->
            case result of
                Ok newLocation ->
                    ( { model | locality = setLocalityFromZip newLocation model.locality }, Cmd.batch [ fetchLocationImage ( newLocation.place.latitude, newLocation.place.longitude ), fetchWeeklyWeather ( newLocation.place.latitude, newLocation.place.longitude ) ] )

                Err _ ->
                    ( model, Cmd.none )

        ReceivedLocationFromIP result ->
            case result of
                Ok newLocation ->
                    ( { model | locality = setLocalityFromIP newLocation model.locality }, Cmd.batch [ fetchLocationImage ( String.fromFloat newLocation.latitude, String.fromFloat newLocation.longitude ), fetchWeeklyWeather ( String.fromFloat newLocation.latitude, String.fromFloat newLocation.longitude ) ] )

                Err _ ->
                    ( model, Cmd.none )

        ReceivedLocationImage result ->
            case result of
                Ok newImages ->
                    ( { model | locality = setPhoto newImages model.locality }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ReceivedWeather result ->
            case result of
                Ok newWeather ->
                    ( { model | weather = newWeather }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ChangeZipInput text ->
            ( { model | zipInput = text }, fetchLocationFromZip text )



-- VIEW


weatherToday : Model -> Weather
weatherToday model =
    case List.head model.weather of
        Just first ->
            first

        Nothing ->
            Weather 0 "-" "-"


weatherDay : Weather -> Weather -> Html Msg
weatherDay we1 we2 =
    li [ class "weather-forecast__period" ]
        [ p [ class "weather-forecast__period__name" ] []
        , svg [ class "weather-icon weather-forecast__period__icon" ] []
        , p []
            [ span [ class "weather-forecast__period__temperature weather-forecast__period__temperature--high" ]
                [ strong []
                    [ text (String.fromInt we1.temperature ++ "°F")
                    ]
                ]
            , span [ class "weather-forecast__period__temperature weather-forecast__period__temperature--low" ] []
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ div [ class "content" ]
            [ main_ []
                [ section [ class "current-weather", style "background-image" ("url(" ++ model.locality.photo ++ ")") ]
                    [ svgWave
                    , div [ class "current-weather__container" ]
                        [ header [ class "current-weather__header" ]
                            [ button [ class "current-weather__header__button" ]
                                [ img [ class "current-weather__header__icon", src "menu.svg" ] [] ]
                            ]
                        , div [ class "current-weather__grid" ]
                            [ span [ class "current-weather__location" ]
                                [ text (model.locality.city ++ ", ")
                                , strong [] [ text model.locality.region ]
                                ]
                            , span [ class "current-weather__date" ] []
                            , span [ class "current-weather__forecast" ]
                                [ text (weatherToday model).condition
                                ]
                            ]
                        , span [ class "current-weather__temperature" ]
                            [ text (String.fromInt (weatherToday model).temperature)
                            , sup [ class "current-weather__temperature__symbol" ]
                                [ text ("°" ++ (weatherToday model).unitForTemp)
                                ]
                            ]
                        ]
                    ]
                , section [ class "weather-forecast" ]
                    [ div [ class "weather-forecast__container" ]
                        [ ul [] []
                        ]
                    ]
                ]
            , aside [] []
            ]
        , div [ class "loading" ] []
        ]


svgWave : Html msg
svgWave =
    svg [ viewBox "0 0 100 17", style "position" "absolute", style "width" "100%", style "bottom" "0", style "left" "0" ]
        [ path [ d "M0 30 V15 Q30 3 60 15 V30z", style "fill" "#00adcf" ]
            []
        , text "          "
        , path [ d "M0 30 V12 Q30 17 55 12 T100 11 V30z", style "fill" "#ffffff" ]
            []
        , text "        "
        ]



--HTTP


locationZipEndpoint : String
locationZipEndpoint =
    "http://api.zippopotam.us/us"


locationIPEndpoint =
    "http://ip-api.com/json"


cityImageEndpoint =
    "https://api.teleport.org/api/locations/"


weatherEndpoint =
    "https://api.weather.gov/points/"


weatherIconEndpoint =
    "https://www.flaticon.com/packs/weather-forecast"


type alias LocationFromZip =
    { zip : String, country : String, place : Place }


type alias LocationFromIP =
    { zip : String, country : String, latitude : Float, longitude : Float, city : String, region : String }


type alias Place =
    { latitude : String, longitude : String, city : String }


type alias Images =
    { mobile : String, web : String }


placeDecoder : Decoder Place
placeDecoder =
    Decode.succeed Place
        |> required "latitude" Decode.string
        |> required "longitude" Decode.string
        |> required "place name" Decode.string


locationZipDecoder : Decoder LocationFromZip
locationZipDecoder =
    Decode.succeed LocationFromZip
        |> required "post code" Decode.string
        |> required "country" Decode.string
        |> required "places" (Decode.index 0 placeDecoder)


locationIPDecoder : Decoder LocationFromIP
locationIPDecoder =
    Decode.succeed LocationFromIP
        |> required "zip" Decode.string
        |> required "country" Decode.string
        |> required "lat" Decode.float
        |> required "lon" Decode.float
        |> required "city" Decode.string
        |> required "region" Decode.string


imagesDecoder : Decoder Images
imagesDecoder =
    Decode.succeed Images
        |> required "mobile" Decode.string
        |> required "web" Decode.string


locationImageDataDecoder : Decoder Images
locationImageDataDecoder =
    Decode.at [ "_embedded", "location:nearest-urban-areas" ] <|
        Decode.index 0 <|
            Decode.at [ "_embedded", "location:nearest-urban-area", "_embedded", "ua:images", "photos" ] <|
                Decode.index 0 <|
                    Decode.field "image" imagesDecoder


weatherListDecoder : Decoder (List Weather)
weatherListDecoder =
    Decode.at [ "properties", "periods" ] <|
        Decode.list
            (Decode.succeed Weather
                |> required "temperature" Decode.int
                |> required "temperatureUnit" Decode.string
                |> required "shortForecast" Decode.string
            )


fetchLocationFromZip : String -> Cmd Msg
fetchLocationFromZip zip =
    Http.send ReceivedLocationFromZip <|
        Http.get (locationZipEndpoint ++ "/" ++ zip) locationZipDecoder


fetchLocationFromIP : Cmd Msg
fetchLocationFromIP =
    Http.send ReceivedLocationFromIP <|
        Http.get locationIPEndpoint locationIPDecoder


fetchLocationImage : ( String, String ) -> Cmd Msg
fetchLocationImage ( lat, lng ) =
    let
        fetchURL =
            cityImageEndpoint ++ lat ++ "," ++ lng ++ "/?embed=location:nearest-urban-areas/location:nearest-urban-area/ua:images/"
    in
    Http.send ReceivedLocationImage <|
        Http.get fetchURL locationImageDataDecoder


fetchWeeklyWeather : ( String, String ) -> Cmd Msg
fetchWeeklyWeather ( lat, lng ) =
    let
        fetchURL =
            weatherEndpoint ++ lat ++ "," ++ lng ++ "/forecast"
    in
    Http.send ReceivedWeather <|
        Http.get fetchURL weatherListDecoder


fetchHourlyWeather : ( String, String ) -> Cmd Msg
fetchHourlyWeather ( lat, lng ) =
    let
        fetchURL =
            weatherEndpoint ++ lat ++ "," ++ lng ++ "/forecast/hourly"
    in
    Http.send ReceivedWeather <|
        Http.get fetchURL weatherListDecoder



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
