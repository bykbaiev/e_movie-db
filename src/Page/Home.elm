module Page.Home exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Api exposing (baseUrl)
import Css exposing (..)
import Genre exposing (Genre, GenresResults)
import Html.Styled exposing (Html, button, div, h1, header, input, span, text)
import Html.Styled.Attributes exposing (class, css, value)
import Html.Styled.Events exposing (keyCode, on, onClick, onInput)
import Html.Styled.Keyed
import Html.Styled.Lazy exposing (lazy, lazy5)
import Http
import Json.Decode as D exposing (Decoder, Value, succeed)
import Json.Decode.Pipeline as DP
import Movie exposing (PreviewMovie)
import MovieId exposing (MovieId)
import Ports exposing (onFavoriteMoviesChange, storeFavoriteMovies, storeQuery)
import RequestHelpers exposing (handleJsonResponse)
import SearchOptions exposing (updateOptions)
import Session exposing (Session)
import String
import StyledDocument exposing (StyledDocument)
import Task exposing (Task)



-- TYPES


type Tab
    = Main
    | Favorite
    | Recommendations


type alias MovieFeed =
    { movies : List PreviewMovie
    , page : Int
    , totalPages : Int
    , totalResults : Int
    }



-- MODEL


type alias Model =
    { session : Session
    , feed : MovieFeed
    , errorMessage : Maybe String
    , searchOptions : SearchOptions.Options
    , tab : Tab
    , genres : GenresResults
    }


initialFeed : MovieFeed
initialFeed =
    { movies = []
    , page = 1
    , totalResults = 0
    , totalPages = 0
    }


initialTab : Tab
initialTab =
    Main


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , feed = initialFeed
      , errorMessage = Nothing
      , searchOptions = SearchOptions.initialModel
      , tab = initialTab
      , genres = []
      }
    , Cmd.batch
        [ fetchFeed session initialTab SearchOptions.initialModel initialFeed.page
            |> Task.attempt GotFeed
        , Task.attempt GotGenres <| Genre.fetch session
        ]
    )



-- UPDATE


type Msg
    = ChangedQuery String
    | Search
    | GotFeed (Result Http.Error MovieFeed)
    | GotGenres (Result Http.Error GenresResults)
    | Options SearchOptions.Msg
    | ChangedPage Int
    | ChangedTab Tab
    | ChangedFavoriteMovie MovieId
      -- | ChangedFavoriteMovies (List MovieId)
    | RemovedFavoriteMovie MovieId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search ->
            ( { model | errorMessage = Nothing }
            , fetchFeed model.session model.tab model.searchOptions 1
                |> Task.attempt GotFeed
            )

        ChangedQuery query ->
            ( model, storeQuery query )

        Options searchOptionsMsg ->
            let
                ( searchOptions, shouldReload ) =
                    updateOptions searchOptionsMsg model.searchOptions

                cmd =
                    if shouldReload then
                        fetchFeed model.session model.tab searchOptions 1
                            |> Task.attempt GotFeed

                    else
                        Cmd.none
            in
            ( { model | searchOptions = searchOptions }
            , cmd
            )

        ChangedPage page ->
            ( model
            , fetchFeed model.session model.tab model.searchOptions page
                |> Task.attempt GotFeed
            )

        GotFeed feedResults ->
            case feedResults of
                Err error ->
                    ( { model | errorMessage = Just <| RequestHelpers.toString error }, Cmd.none )

                Ok feed ->
                    ( { model | feed = feed, errorMessage = Nothing }, Cmd.none )

        ChangedTab tab ->
            ( { model | tab = tab }, Cmd.none )

        GotGenres genresResults ->
            case genresResults of
                Err error ->
                    ( { model | errorMessage = Just <| RequestHelpers.toString error }, Cmd.none )

                Ok genres ->
                    ( { model | genres = genres, errorMessage = Nothing }, Cmd.none )

        ChangedFavoriteMovie id ->
            let
                favoriteMovies =
                    id :: Session.favoriteMovies model.session
            in
            ( model, storeFavorite favoriteMovies )

        RemovedFavoriteMovie id ->
            let
                favoriteMovies =
                    List.filter (\movieId -> movieId /= id) (Session.favoriteMovies model.session)
            in
            ( model, storeFavorite favoriteMovies )


storeFavorite : List MovieId -> Cmd Msg
storeFavorite ids =
    storeFavoriteMovies <| List.map MovieId.toInt ids



-- VIEW


view : Model -> StyledDocument Msg
view model =
    { title = "Home page - MovieDB"
    , body =
        [ div
            [ css
                [ width (px 960)
                , margin2 zero auto
                , padding2 (px 32) zero
                , fontFamilies [ "Helvetica", "Arial", "serif" ]
                ]
            ]
            [ header
                [ css
                    [ position relative
                    , padding2 (px 8) (px 16)
                    , backgroundColor (rgb 96 181 204)
                    , height (px 36)
                    ]
                ]
                [ h1 [] [ text "MovieDB" ]
                , span
                    [ css
                        [ position absolute
                        , top (px 16)
                        , right (px 16)
                        , fontStyle italic
                        , fontSize (px 24)
                        , backgroundColor (hex "#eee")
                        ]
                    ]
                    [ text "Search for the movies accross different databases" ]
                ]
            , input [ class "search-query", onInput ChangedQuery, value <| Session.query model.session, onEnter Search ] []
            , button [ class "search-button", onClick Search ] [ text "Search" ]
            , Html.Styled.map Options (lazy SearchOptions.view model.searchOptions)
            , viewErrorMessage model.errorMessage
            , Html.Styled.Keyed.node
                "div"
                [ class "results" ]
                (List.map (viewKeyedSearchResult model.genres (Session.favoriteMovies model.session)) model.feed.movies)
            , lazy viewPagination { page = model.feed.page, total = model.feed.totalPages }
            ]
        ]
    }


viewErrorMessage : Maybe String -> Html Msg
viewErrorMessage errorMessage =
    case errorMessage of
        Just message ->
            div [ class "error" ] [ text message ]

        Nothing ->
            text ""


viewKeyedSearchResult : GenresResults -> List MovieId -> PreviewMovie -> ( String, Html Msg )
viewKeyedSearchResult genres favoriteMovies movie =
    ( MovieId.toString <| Movie.id movie
    , lazy5 Movie.view movie genres favoriteMovies ChangedFavoriteMovie RemovedFavoriteMovie
    )


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
        , onClick <| ChangedPage page
        ]
        [ text <| String.fromInt page ]


viewPaginationSpace : Html msg
viewPaginationSpace =
    div [] [ text ". . ." ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- FETCH


fetchFeed : Session -> Tab -> SearchOptions.Options -> Int -> Task Http.Error MovieFeed
fetchFeed session tab options page =
    let
        regionQuery =
            RequestHelpers.queryParam
                { name = "region"
                , value = Maybe.withDefault "" options.region
                , isFirst = False
                }

        languageQuery =
            RequestHelpers.queryParam
                { name = "language"
                , value = Maybe.withDefault "" options.language
                , isFirst = False
                }

        pageQuery =
            RequestHelpers.queryParam
                { name = "page"
                , value = String.fromInt page
                , isFirst = False
                }

        url =
            case tab of
                Main ->
                    baseUrl
                        ++ "search/movie?"
                        ++ regionQuery
                        ++ languageQuery
                        ++ pageQuery

                Favorite ->
                    ""

                Recommendations ->
                    ""
    in
    Http.task
        { method = "GET"
        , headers = []
        , url = Session.withQuery session <| Session.withToken session url
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| feedDecoder
        , timeout = Nothing
        }



-- SERIALIZATION


feedDecoder : Decoder MovieFeed
feedDecoder =
    succeed MovieFeed
        |> DP.required "results" (D.list Movie.previewDecoder)
        |> DP.required "page" D.int
        |> DP.required "total_pages" D.int
        |> DP.required "total_results" D.int



-- EVENTS


onEnter : Msg -> Html.Styled.Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                D.succeed msg

            else
                D.fail "not ENTER"
    in
    on "keydown" (D.andThen isEnter keyCode)



-- STYLES AND COLORS


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
