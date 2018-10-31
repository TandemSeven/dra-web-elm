module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
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
    ( Model "" (Locality "00000" "" "" "" "") (Weather "" ""), Cmd.none )



-- UPDATE


type Msg
    = Fetch String
    | Data (Result Http.Error Location)


setZip : String -> Locality -> Locality
setZip newZip locality =
    { locality | zip = newZip }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch url ->
            ( model, doFetch url )

        Data result ->
            case result of
                Ok newLocation ->
                    ( { model | locality = setZip newLocation.zip model.locality }, Cmd.none )

                Err _ ->
                    ( { model | locality = setZip "XXXXX" model.locality }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "margin" "0" ]
        [ text ("ZIP:" ++ model.locality.zip), button [ onClick (Fetch "01545") ] [ text "Fetch" ] ]


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


cityImageEndpoint =
    "https://api.teleport.org/api/locations"


weatherEndpoint =
    "https://api.weather.gov/points"


weatherIconEndpoint =
    "https://www.flaticon.com/packs/weather-forecast"


type alias Location =
    { zip : String, country : String, places : List Place }


type alias Place =
    { latitude : String, longitude : String, city : String }


placeDecoder : Decoder Place
placeDecoder =
    Decode.succeed Place
        |> required "latitude" Decode.string
        |> required "longitude" Decode.string
        |> required "place name" Decode.string


locationDecoder : Decoder Location
locationDecoder =
    Decode.succeed Location
        |> required "post code" Decode.string
        |> required "country" Decode.string
        |> required "places" (Decode.list placeDecoder)


doFetch : String -> Cmd Msg
doFetch zip =
    Http.send Data <|
        Http.get (locationZipEndpoint ++ "/" ++ zip) locationDecoder



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
