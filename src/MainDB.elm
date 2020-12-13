module MainDB exposing (..)

import Css exposing (..)
import Html.Styled exposing (Html, button, div, h1, header, input, li, span, text)
import Html.Styled.Attributes exposing (class, css, value)
import Html.Styled.Events exposing (keyCode, on, onClick, onInput)
import Html.Styled.Keyed
import Html.Styled.Lazy exposing (lazy)
import Http
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline as DPipeline
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
    , results : SearchResults
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
    | Search
    | HandleSearchResults (Result Http.Error SearchResults)
    | Options SearchOptions.Msg
    | SetPage Int


initialModel : Model
initialModel =
    { query = "Gentlemen"
    , results =
        { movies = []
        , page = 1
        , totalResults = 0
        , totalPages = 0
        }
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
    , searchMovies initialQuery apiToken initialModel.searchOptions initialModel.results.page
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search ->
            if model.query == "" then
                ( { model | errorMessage = Just "Search query cannot be empty" }, Cmd.none )

            else
                ( { model | errorMessage = Nothing }
                , searchMovies model.query model.apiToken model.searchOptions 1
                )

        SetQuery query ->
            ( { model | query = query }, storeQuery query )

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
                ( searchOptions, shouldReload ) =
                    updateOptions searchOptionsMsg model.searchOptions

                cmd =
                    if shouldReload then
                        searchMovies model.query model.apiToken searchOptions 1

                    else
                        Cmd.none
            in
            ( { model | searchOptions = searchOptions }
            , cmd
            )

        SetPage page ->
            ( model, searchMovies model.query model.apiToken model.searchOptions page )


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
        , Html.Styled.Keyed.node "ul" [ class "results" ] (List.map viewKeyedSearchResult model.results.movies)
        , lazy viewPagination { page = model.results.page, total = model.results.totalPages }
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
        ]


paginationContainerStyle : List Style
paginationContainerStyle =
    [ displayFlex
    , justifyContent center
    , alignItems center
    ]


paginationColorGreen : Color
paginationColorGreen =
    hex "#4e8d7c"


paginationColorWhite : Color
paginationColorWhite =
    hex "#fff"


paginationCellBorderColor : Color
paginationCellBorderColor =
    hex "#e6e6e6"


viewPagination : { page : Int, total : Int } -> Html Msg
viewPagination { page, total } =
    let
        range =
            if total < 10 then
                List.range 1 total

            else
                List.filter
                    (\x -> x > 0 && x < total + 1)
                    [ page - 2, page - 1, page, page + 1, page + 2 ]

        isFirstIncluded =
            List.member 1 range

        isLastIncluded =
            List.member total range

        firstContainer =
            if not isFirstIncluded then
                [ viewPaginationCell page 1
                , viewPaginationSpace
                ]

            else
                [ text "" ]

        lastContainer =
            if not isLastIncluded then
                [ viewPaginationSpace
                , viewPaginationCell page total
                ]

            else
                [ text "" ]

        rangeContainer =
            div [ css paginationContainerStyle ] <| List.map (viewPaginationCell page) range
    in
    if total == 0 then
        div [] []

    else
        div
            [ css paginationContainerStyle ]
            (firstContainer
                ++ rangeContainer
                :: lastContainer
            )


viewPaginationCell : Int -> Int -> Html Msg
viewPaginationCell selectedPage page =
    let
        selected =
            selectedPage == page

        selectedStyle =
            if selected then
                [ backgroundColor paginationColorGreen
                , color paginationColorWhite
                ]

            else
                []
    in
    div
        [ css <|
            [ margin2 zero (px 8)
            , width <| px 40
            , height <| px 40
            , lineHeight <| px 40
            , color paginationColorGreen
            , backgroundColor paginationColorWhite
            , textAlign center
            , border3 (px 1) solid paginationCellBorderColor
            , cursor pointer
            ]
                ++ selectedStyle
        , onClick <| SetPage page
        ]
        [ text <| String.fromInt page ]


viewPaginationSpace : Html msg
viewPaginationSpace =
    div [] [ text ". . ." ]


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


searchMovies : String -> Maybe String -> SearchOptions.Options -> Int -> Cmd Msg
searchMovies query token options page =
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
                    ++ "&page="
                    ++ String.fromInt page
    in
    -- if isEmptyToken then
    --     Cmd. HandleSearchResults (Err (Http.BadUrl "There is no API token"))
    -- else
    Http.get
        { url = url
        , expect = Http.expectJson HandleSearchResults searchMoviesDecoder
        }


type alias SearchResults =
    { movies : List Movie
    , page : Int
    , totalPages : Int
    , totalResults : Int
    }


searchMoviesDecoder : Decoder SearchResults
searchMoviesDecoder =
    Json.Decode.succeed SearchResults
        |> DPipeline.required "results" (Json.Decode.list movieDecoder)
        |> DPipeline.required "page" Json.Decode.int
        |> DPipeline.required "total_pages" Json.Decode.int
        |> DPipeline.required "total_results" Json.Decode.int


movieDecoder : Decoder Movie
movieDecoder =
    Json.Decode.succeed Movie
        |> DPipeline.required "id" Json.Decode.int
        |> DPipeline.required "title" Json.Decode.string
        |> DPipeline.required "vote_average" Json.Decode.float
