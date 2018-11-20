module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, aside, button, div, form, h2, header, img, input, li, main_, p, section, span, strong, sup, text, ul)
import Html.Attributes exposing (class, href, placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Set exposing (Set)
import SvgAssets exposing (..)
import Task
import Time
import Url
import Util exposing (clockDisplay, findStringIn, getListInPairs)
import Weather exposing (Weather, getWeatherForDay, weatherConditionIcon, weatherToday)



-- App Architecture


main =
    Browser.application
        { init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Locality =
    { zip : String, city : String, region : String, photo : String, lat : String, lng : String }


type alias LocationFromZip =
    { zip : String, country : String, place : Place }


type alias LocationFromIP =
    { zip : String, country : String, latitude : Float, longitude : Float, city : String, region : String }


type alias Place =
    { latitude : String, longitude : String, city : String, region : String }


type alias Images =
    { mobile : String, web : String }


type alias Model =
    { zipInput : String
    , locality : Locality
    , weather : List Weather
    , menuOpen : Bool
    , searchedZips : Set String
    , loading : Bool
    , timezone : Time.Zone
    , time : Time.Posix
    , key : Nav.Key
    , url : Url.Url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model "" (Locality "" "" "" "" "" "") [] False Set.empty True Time.utc (Time.millisToPosix 0) key url
    , Cmd.batch [ fetchLocationFromIP, Task.perform AdjustTimeZone Time.here, Task.perform Tick Time.now ]
    )



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
    | ClearSearchedZips
    | OpenMenu
    | CloseMenu
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


setLocalityFromZip : LocationFromZip -> Locality -> Locality
setLocalityFromZip location locality =
    { locality
        | zip = location.zip
        , city = location.place.city
        , region = location.place.region
        , lat = location.place.latitude
        , lng = location.place.longitude
    }


setLocalityFromIP : LocationFromIP -> Locality -> Locality
setLocalityFromIP location locality =
    { locality
        | zip = location.zip
        , city = location.city
        , region = location.region
        , lat = String.fromFloat location.latitude
        , lng = String.fromFloat location.longitude
    }


setPhoto : Images -> Locality -> Locality
setPhoto newImages locality =
    { locality | photo = newImages.web }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchLocationFromZip zip ->
            ( { model | menuOpen = False, loading = True }, fetchLocationFromZip zip )

        FetchLocationFromIP ->
            ( { model | menuOpen = False, loading = True }, fetchLocationFromIP )

        FetchLocationImage ( lat, lng ) ->
            ( model, fetchLocationImage ( lat, lng ) )

        FetchWeather ( lat, lng ) ->
            ( model, fetchWeeklyWeather ( lat, lng ) )

        ReceivedLocationFromZip result ->
            case result of
                Ok newLocation ->
                    ( { model
                        | locality = setLocalityFromZip newLocation model.locality
                        , searchedZips = Set.insert newLocation.zip model.searchedZips
                        , zipInput = ""
                      }
                    , Cmd.batch
                        [ fetchLocationImage ( newLocation.place.latitude, newLocation.place.longitude )
                        , fetchWeeklyWeather ( newLocation.place.latitude, newLocation.place.longitude )
                        ]
                    )

                Err _ ->
                    ( { model | loading = False }, Cmd.none )

        ReceivedLocationFromIP result ->
            case result of
                Ok newLocation ->
                    ( { model | locality = setLocalityFromIP newLocation model.locality }
                    , Cmd.batch
                        [ fetchLocationImage ( String.fromFloat newLocation.latitude, String.fromFloat newLocation.longitude )
                        , fetchWeeklyWeather ( String.fromFloat newLocation.latitude, String.fromFloat newLocation.longitude )
                        ]
                    )

                Err _ ->
                    ( { model | loading = False }, Cmd.none )

        ReceivedLocationImage result ->
            case result of
                Ok newImages ->
                    ( { model | locality = setPhoto newImages model.locality }, Cmd.none )

                Err _ ->
                    ( { model | loading = False }, Cmd.none )

        ReceivedWeather result ->
            case result of
                Ok newWeather ->
                    ( { model | weather = newWeather, loading = False }, Cmd.none )

                Err _ ->
                    ( { model | loading = False }, Cmd.none )

        ChangeZipInput text ->
            ( { model | zipInput = text }, Cmd.none )

        ClearSearchedZips ->
            ( { model | searchedZips = Set.empty }, Cmd.none )

        OpenMenu ->
            ( { model | menuOpen = True }, Cmd.none )

        CloseMenu ->
            ( { model | menuOpen = False }, Cmd.none )

        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | timezone = newZone }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        -- http://localhost:3000/#/settings
        UrlChanged url ->
            ( { model | menuOpen = True }
            , Cmd.none
            )



-- VIEW


loading : Model -> Html Msg
loading model =
    if model.loading then
        div [ class "loading loading--visible" ]
            [ svgRefresh
            ]

    else
        div [ class "loading" ]
            []


sidebar : Model -> Html Msg
sidebar model =
    aside
        [ if model.menuOpen then
            class "sidebar sidebar--opened"

          else
            class "sidebar"
        ]
        [ button [ class "sidebar__close", onClick CloseMenu ] [ svgClose "sidebar__icon" ]
        , form [ class "sidebar__form", onSubmit (FetchLocationFromZip model.zipInput) ]
            [ input
                [ class "input sidebar__form__input"
                , placeholder "Enter Zip Code"
                , onInput ChangeZipInput
                , value model.zipInput
                ]
                []
            , button [ class "sidebar__form__change button button--primary", type_ "submit" ] [ text "Search" ]
            , button [ class "sidebar__form__current button button--tertiary", type_ "button", onClick FetchLocationFromIP ] [ text "Current Location" ]
            ]
        , if Set.size model.searchedZips > 0 then
            div [ class "recent-searches" ]
                [ h2 [] [ text "Recent Zip Code Searches:" ]
                , ul []
                    (List.map
                        (\el -> li [ onClick (FetchLocationFromZip el) ] [ a [ href "#" ] [ text el ] ])
                        (Set.toList model.searchedZips)
                    )
                , button [ class "button button--secondary", onClick ClearSearchedZips ] [ text "Clear Recent Zip Codes" ]
                ]

          else
            div [] []
        ]


hero : Model -> Html Msg
hero model =
    section [ class "current-weather", style "background-image" ("url(" ++ model.locality.photo ++ ")") ]
        [ svgWave
        , div [ class "current-weather__container" ]
            [ header [ class "current-weather__header" ]
                [ button [ class "current-weather__header__button", onClick OpenMenu ]
                    [ svgMenu ]
                ]
            , div [ class "current-weather__grid" ]
                [ span [ class "current-weather__location" ]
                    [ text (model.locality.city ++ ", ")
                    , strong [] [ text model.locality.region ]
                    ]
                , span [ class "current-weather__date" ]
                    [ text <| clockDisplay model.timezone model.time ]
                , span [ class "current-weather__forecast" ]
                    [ text (weatherToday model.weather).condition
                    ]
                ]
            , span [ class "current-weather__temperature" ]
                [ div
                    [ class "weather-icon weather-forecast__period__icon" ]
                    [ weatherConditionIcon (weatherToday model.weather) ]
                , span [ class "current-weather__temperature__value" ]
                    [ text (String.fromInt (weatherToday model.weather).temperature)
                    , sup [ class "current-weather__temperature__symbol" ]
                        [ text ("°" ++ (weatherToday model.weather).unitForTemp)
                        ]
                    ]
                ]
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "DRA Weather App"
    , body =
        [ div [ class "app" ]
            [ div [ class "content" ]
                [ main_ []
                    [ hero model
                    , section [ class "weather-forecast" ]
                        [ div [ class "weather-forecast__container" ]
                            [ ul [] (List.map (getWeatherForDay model.timezone) (getListInPairs model.weather []))
                            ]
                        ]
                    ]
                , sidebar model
                ]
            , loading model
            ]
        ]
    }



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


placeDecoder : Decoder Place
placeDecoder =
    Decode.succeed Place
        |> required "latitude" Decode.string
        |> required "longitude" Decode.string
        |> required "place name" Decode.string
        |> required "state abbreviation" Decode.string


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
                |> required "startTime" Iso8601.decoder
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
    Time.every (1000 * 60) Tick
