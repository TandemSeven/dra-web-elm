module Main exposing (main)

import Browser
import Html exposing (Html, button, div, span, text)
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
    { temperature : String, condition : String }


type alias Locality =
    { zip : String, city : String, photo : String, lat : String, lng : String }


type alias Model =
    { time : String, locality : Locality, weather : Weather }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" (Locality "00000" "" "" "" "") (Weather "" ""), fetchLocationFromIP )



-- UPDATE


type Msg
    = FetchLocationFromZip String
    | FetchLocationFromIP
    | ReceivedLocationFromZip (Result Http.Error LocationFromZip)
    | ReceivedLocationFromIP (Result Http.Error LocationFromIP)


setLocalityFromZip : LocationFromZip -> Locality -> Locality
setLocalityFromZip location locality =
    let
        place =
            case List.head location.places of
                Just firstPlace ->
                    firstPlace

                Nothing ->
                    Place "" "" ""
    in
    { locality | zip = location.zip, city = place.city, lat = place.latitude, lng = place.longitude }


setLocalityFromIP : LocationFromIP -> Locality -> Locality
setLocalityFromIP location locality =
    { locality | zip = location.zip, city = location.city, lat = String.fromFloat location.latitude, lng = String.fromFloat location.longitude }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchLocationFromZip url ->
            ( model, fetchLocationFromZip url )

        FetchLocationFromIP ->
            ( model, fetchLocationFromIP )

        ReceivedLocationFromZip result ->
            case result of
                Ok newLocation ->
                    ( { model | locality = setLocalityFromZip newLocation model.locality }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ReceivedLocationFromIP result ->
            case result of
                Ok newLocation ->
                    ( { model | locality = setLocalityFromIP newLocation model.locality }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "margin" "0", style "display" "flex", style "flex-direction" "column", style "justify-content" "center" ]
        [ text ("ZIP:" ++ model.locality.zip), button [ onClick (FetchLocationFromZip "60606"), style "max-width" "50px" ] [ text "Fetch" ], span [] [ text ("City: " ++ model.locality.city) ], text (" | LatLng: " ++ model.locality.lat ++ ", " ++ model.locality.lng) ]


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
    "https://api.teleport.org/api/locations"


weatherEndpoint =
    "https://api.weather.gov/points"


weatherIconEndpoint =
    "https://www.flaticon.com/packs/weather-forecast"


type alias LocationFromZip =
    { zip : String, country : String, places : List Place }


type alias LocationFromIP =
    { zip : String, country : String, latitude : Float, longitude : Float, city : String }


type alias Place =
    { latitude : String, longitude : String, city : String }


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
        |> required "places" (Decode.list placeDecoder)


locationIPDecoder : Decoder LocationFromIP
locationIPDecoder =
    Decode.succeed LocationFromIP
        |> required "zip" Decode.string
        |> required "country" Decode.string
        |> required "lat" Decode.float
        |> required "lon" Decode.float
        |> required "city" Decode.string


fetchLocationFromZip : String -> Cmd Msg
fetchLocationFromZip zip =
    Http.send ReceivedLocationFromZip <|
        Http.get (locationZipEndpoint ++ "/" ++ zip) locationZipDecoder


fetchLocationFromIP : Cmd Msg
fetchLocationFromIP =
    Http.send ReceivedLocationFromIP <|
        Http.get locationIPEndpoint locationIPDecoder



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
