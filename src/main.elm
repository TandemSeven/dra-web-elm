module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, viewBox)



-- App Architecture


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Weather =
    { highTemp : Int, unitForTemp : String, condition : String }


type alias Locality =
    { zip : String, city : String, photo : String, lat : String, lng : String }


type alias Model =
    { zipInput : String, locality : Locality, weather : List Weather }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "00000" (Locality "00000" "" "" "" "") [], fetchLocationFromIP )



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
    { locality | zip = location.zip, city = location.city, lat = String.fromFloat location.latitude, lng = String.fromFloat location.longitude }


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
                    ( { model | locality = setLocalityFromZip newLocation model.locality }, fetchLocationImage ( newLocation.place.latitude, newLocation.place.longitude ) )

                Err _ ->
                    ( model, Cmd.none )

        ReceivedLocationFromIP result ->
            case result of
                Ok newLocation ->
                    ( { model | locality = setLocalityFromIP newLocation model.locality }, fetchLocationImage ( String.fromFloat newLocation.latitude, String.fromFloat newLocation.longitude ) )

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


view : Model -> Html Msg
view model =
    div [ style "margin" "0", style "display" "flex", style "flex-direction" "column", style "justify-content" "center" ]
        [ div []
            [ button [ onClick (FetchLocationFromZip "60606"), style "max-width" "150px" ] [ text "Chicago" ]
            , button [ onClick (FetchWeather ( model.locality.lat, model.locality.lng )), style "max-width" "150px" ] [ text "Weather" ]
            ]
        , input [ type_ "text", onInput ChangeZipInput, placeholder "ZIP Code", value model.zipInput ] []
        , text ("ZIP:" ++ model.locality.zip)
        , span [] [ text ("City: " ++ model.locality.city) ]
        , span [] [ text ("LatLng: " ++ model.locality.lat ++ ", " ++ model.locality.lng) ]
        , span [] [ text ("Image: " ++ model.locality.photo) ]
        , div []
            (List.map
                (\day -> div [] [ text (String.fromInt day.highTemp) ])
                model.weather
            )
        ]


svgWave : Html msg
svgWave =
    svg [ viewBox "0 0 100 17" ]
        [ path [ d "M0 30 V15 Q30 3 60 15 V30z" ]
            []
        , text "          "
        , path [ d "M0 30 V12 Q30 17 55 12 T100 11 V30z" ]
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
    { zip : String, country : String, latitude : Float, longitude : Float, city : String }


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
    Decode.at [ "properties" ] <|
        Decode.field "periods"
            (Decode.list
                (Decode.succeed Weather
                    |> required "temperature" Decode.int
                    |> required "temperatureUnit" Decode.string
                    |> required "shortForecast" Decode.string
                )
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
