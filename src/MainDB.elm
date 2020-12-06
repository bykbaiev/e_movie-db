module MainDB exposing (..)

import Auth exposing (apiToken)
import Html exposing (Html, button, div, h1, header, input, li, span, text, ul)
import Html.Attributes exposing (class, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Html.Keyed
import Http
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import String
import Url exposing (baseUrl)


type alias Model =
    { query : String, results : List Movie, errorMessage : Maybe String }


type alias Movie =
    { id : Int, title : String, rate : Float }


type alias MovieError =
    String


type Msg
    = SetQuery String
    | DeleteById Int
    | Search
    | HandleGetMovieError Http.Error
    | HandleGetMovieResponse Movie
    | HandleSearchResults (Result Http.Error (List Movie))
    | KeyDown Int


initialModel : Model
initialModel =
    { query = "Gentlemen"
    , results = []
    , errorMessage = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, searchMovies initialModel.query )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search ->
            ( { model | errorMessage = Nothing }, searchMovies model.query )

        SetQuery query ->
            ( { model | query = query }, Cmd.none )

        DeleteById id ->
            ( { model | results = List.filter (\movie -> movie.id /= id) model.results }, Cmd.none )

        HandleGetMovieResponse _ ->
            ( model, Cmd.none )

        HandleGetMovieError _ ->
            ( model, Cmd.none )

        HandleSearchResults searchResults ->
            case searchResults of
                Err error ->
                    let
                        errorMessage =
                            case error of
                                Http.BadUrl message ->
                                    message

                                Http.Timeout ->
                                    "Timeout is reached"

                                Http.NetworkError ->
                                    "There are some network errors. Please, check your connection"

                                Http.BadStatus status ->
                                    "The requests failed with status code " ++ String.fromInt status

                                Http.BadBody message ->
                                    "The request failed with some bad body: " ++ message
                    in
                    ( { model | errorMessage = Just errorMessage }, Cmd.none )

                Ok results ->
                    ( { model | results = results, errorMessage = Nothing }, Cmd.none )

        KeyDown keyCode ->
            case keyCode of
                13 ->
                    ( { model | errorMessage = Nothing }, searchMovies model.query )

                _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ header []
            [ h1 [] [ text "MovieDB" ]
            , span [ class "tagline" ] [ text "Search for the movies accross different databases" ]
            ]
        , input [ class "search-query", onInput SetQuery, value model.query, onKeyDown KeyDown ] []
        , button [ class "search-button", onClick Search ] [ text "Search" ]
        , viewErrorMessage model.errorMessage
        , Html.Keyed.node "ul" [ class "results" ] (List.map viewSearchResult model.results)
        ]


viewErrorMessage : Maybe String -> Html Msg
viewErrorMessage errorMessage =
    case errorMessage of
        Just message ->
            div [ class "error" ] [ text message ]

        Nothing ->
            text ""


viewSearchResult : Movie -> ( String, Html Msg )
viewSearchResult movie =
    ( String.fromInt movie.id
    , li []
        [ span [ class "star-count" ] [ text (String.fromFloat movie.rate) ]
        , span [ class "title" ] [ text movie.title ]
        , button [ class "hide-movie", onClick (DeleteById movie.id) ]
            [ text "X" ]
        ]
    )


onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
    on "keydown" (Json.Decode.map tagger keyCode)


searchMovies : String -> Cmd Msg
searchMovies query =
    let
        url =
            baseUrl
                ++ "search/movie?api_key="
                ++ apiToken
                ++ "&query="
                ++ query
                ++ "&language=en-US&page=1"
    in
    Http.get
        { url = url
        , expect = Http.expectJson HandleSearchResults searchMoviesDecoder
        }


searchMoviesDecoder : Decoder (List Movie)
searchMoviesDecoder =
    Json.Decode.at [ "results" ] <| Json.Decode.list movieDecoder


movieDecoder : Decoder Movie
movieDecoder =
    Json.Decode.succeed Movie
        |> required "id" Json.Decode.int
        |> required "title" Json.Decode.string
        |> required "vote_average" Json.Decode.float
