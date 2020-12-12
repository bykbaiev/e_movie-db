module MainDB exposing (..)

import Html.Styled exposing (Html, button, div, h1, header, input, li, span, text)
import Html.Styled.Attributes exposing (class, value)
import Html.Styled.Events exposing (keyCode, on, onClick, onInput)
import Html.Styled.Keyed
import Html.Styled.Lazy exposing (lazy)
import Http
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Ports exposing (storeQuery)
import SearchOptions exposing (updateOptions)
import String
import Url exposing (baseUrl)


type alias Flags =
    { query : Maybe String
    , apiToken : Maybe String
    }


type alias Model =
    { query : String
    , results : List Movie
    , errorMessage : Maybe String
    , apiToken : Maybe String
    , searchOptions : SearchOptions.Options
    }


type alias Movie =
    { id : Int
    , title : String
    , rate : Float
    }


type alias MovieError =
    String


type Msg
    = SetQuery String
    | DeleteById Int
    | Search
    | HandleSearchResults (Result Http.Error (List Movie))
    | Options SearchOptions.Msg


initialModel : Model
initialModel =
    { query = "Gentlemen"
    , results = []
    , errorMessage = Nothing
    , apiToken = Nothing
    , searchOptions = SearchOptions.initialModel
    }


init : Flags -> ( Model, Cmd Msg )
init { query, apiToken } =
    let
        initialQuery =
            query
                |> Maybe.map
                    (\v ->
                        if v == "" then
                            initialModel.query

                        else
                            v
                    )
                |> Maybe.withDefault initialModel.query
    in
    ( { initialModel
        | query = initialQuery
        , apiToken = apiToken
      }
    , searchMovies initialQuery apiToken initialModel.searchOptions
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search ->
            if model.query == "" then
                ( { model | errorMessage = Just "Search query cannot be empty" }, Cmd.none )

            else
                ( { model | errorMessage = Nothing }, searchMovies model.query model.apiToken model.searchOptions )

        SetQuery query ->
            ( { model | query = query }, storeQuery query )

        DeleteById id ->
            ( { model | results = List.filter (\movie -> movie.id /= id) model.results }, Cmd.none )

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

        Options searchOptionsMsg ->
            let
                searchOptions =
                    updateOptions searchOptionsMsg model.searchOptions
            in
            ( { model | searchOptions = searchOptions }
            , searchMovies model.query model.apiToken searchOptions
            )


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ header []
            [ h1 [] [ text "MovieDB" ]
            , span [ class "tagline" ] [ text "Search for the movies accross different databases" ]
            ]
        , input [ class "search-query", onInput SetQuery, value model.query, onEnter Search ] []
        , button [ class "search-button", onClick Search ] [ text "Search" ]
        , Html.Styled.map Options (lazy SearchOptions.view model.searchOptions)
        , viewErrorMessage model.errorMessage
        , Html.Styled.Keyed.node "ul" [ class "results" ] (List.map viewKeyedSearchResult model.results)
        ]


viewErrorMessage : Maybe String -> Html Msg
viewErrorMessage errorMessage =
    case errorMessage of
        Just message ->
            div [ class "error" ] [ text message ]

        Nothing ->
            text ""


viewKeyedSearchResult : Movie -> ( String, Html Msg )
viewKeyedSearchResult movie =
    ( String.fromInt movie.id
    , lazy viewSearchResult movie
    )


viewSearchResult : Movie -> Html Msg
viewSearchResult movie =
    li []
        [ span [ class "star-count" ] [ text (String.fromFloat movie.rate) ]
        , span [ class "title" ] [ text movie.title ]
        , button [ class "hide-movie", onClick (DeleteById movie.id) ]
            [ text "X" ]
        ]


onEnter : Msg -> Html.Styled.Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.Decode.succeed msg

            else
                Json.Decode.fail "not ENTER"
    in
    on "keydown" (Json.Decode.andThen isEnter keyCode)


searchMovies : String -> Maybe String -> SearchOptions.Options -> Cmd Msg
searchMovies query token options =
    let
        isEmptyToken =
            Maybe.withDefault "" token == ""

        regionQuery =
            case options.region of
                Just region ->
                    "&region=" ++ region

                Nothing ->
                    ""

        languageQuery =
            case options.language of
                Just language ->
                    "&language=" ++ language

                Nothing ->
                    ""

        includeAdultQuery =
            "&include_adult="
                ++ (if options.includeAdult then
                        "true"

                    else
                        "false"
                   )

        url =
            if isEmptyToken then
                ""

            else
                baseUrl
                    ++ "search/movie?api_key="
                    ++ Maybe.withDefault "" token
                    ++ "&query="
                    ++ query
                    ++ regionQuery
                    ++ languageQuery
                    ++ includeAdultQuery
                    ++ "&page=1"
    in
    -- if isEmptyToken then
    --     Cmd. HandleSearchResults (Err (Http.BadUrl "There is no API token"))
    -- else
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
