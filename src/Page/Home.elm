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
import Genre exposing (Genre)
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
import Ports exposing (onSessionChange, storeSession)
import Regex exposing (Options)
import RequestHelpers exposing (handleJsonResponse)
import SearchOptions exposing (updateOptions)
import Session exposing (Session, favoriteMovies)
import String
import StyledDocument exposing (StyledDocument)
import Task exposing (Task)



-- TYPES


type Tab
    = Main
    | Favorite
    | Recommendations


type alias Feed =
    { movies : List PreviewMovie
    , page : Int
    , totalPages : Int
    , totalResults : Int
    }


type Status a
    = Loading
    | Success a
    | Failure String



-- MODEL


type alias Model =
    { session : Session
    , query : String
    , searchOptions : SearchOptions.Options -- TODO move logic here, make SearchOptions simple module without TEA
    , tab : Tab
    , feed : Status Feed
    , genres : Status (List Genre)
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session
            , query = ""
            , searchOptions = SearchOptions.initialModel
            , tab = Main
            , feed = Loading
            , genres = Loading
            }
    in
    ( model
    , Cmd.batch
        [ Task.attempt GotFeed <| fetchFeed model 1
        , Task.attempt GotGenres <| Genre.fetch session
        ]
    )



-- UPDATE


type Msg
    = ChangedQuery String
    | Search
    | GotFeed (Result Http.Error Feed)
    | GotGenres (Result Http.Error (List Genre))
    | Options SearchOptions.Msg
    | ChangedPage Int
    | ChangedTab Tab
    | SelectedFavoriteMovie MovieId
    | GotSession Session
    | RemovedFavoriteMovie MovieId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search ->
            ( { model | feed = Loading }
            , Task.attempt GotFeed <| fetchFeed model 1
            )

        ChangedQuery query ->
            ( { model | query = query }, Cmd.none )

        Options searchOptionsMsg ->
            let
                ( searchOptions, shouldReload ) =
                    updateOptions searchOptionsMsg model.searchOptions

                updatedModel =
                    if shouldReload then
                        { model | searchOptions = searchOptions, feed = Loading }

                    else
                        { model | searchOptions = searchOptions }

                cmd =
                    if shouldReload then
                        Task.attempt GotFeed <| fetchFeed updatedModel 1

                    else
                        Cmd.none
            in
            ( updatedModel, cmd )

        ChangedPage page ->
            ( { model | feed = Loading }
            , Task.attempt GotFeed <| fetchFeed model page
            )

        GotFeed feedResults ->
            case feedResults of
                Err error ->
                    ( { model | feed = Failure <| RequestHelpers.toString error }, Cmd.none )

                Ok feed ->
                    ( { model | feed = Success feed }, Cmd.none )

        ChangedTab tab ->
            let
                updatedModel =
                    { model | tab = tab, feed = Loading }
            in
            ( updatedModel
            , Task.attempt GotFeed <| fetchFeed updatedModel 1
            )

        GotGenres genresResults ->
            case genresResults of
                Err error ->
                    ( { model | genres = Failure <| RequestHelpers.toString error }, Cmd.none )

                Ok genres ->
                    ( { model | genres = Success genres }, Cmd.none )

        SelectedFavoriteMovie id ->
            let
                favoriteMovies =
                    id :: Session.favoriteMovies model.session
            in
            ( model, storeFavorite model.session favoriteMovies )

        RemovedFavoriteMovie id ->
            let
                favoriteMovies =
                    List.filter (\movieId -> movieId /= id) (Session.favoriteMovies model.session)
            in
            ( model, storeFavorite model.session favoriteMovies )

        GotSession session ->
            ( { model | session = session }, Cmd.none )


storeFavorite : Session -> List MovieId -> Cmd Msg
storeFavorite session fMovies =
    let
        sessionValue =
            Session.encode session fMovies
    in
    storeSession sessionValue



-- VIEW


view : Model -> StyledDocument Msg
view model =
    { title = "Home page - MovieDB"
    , body =
        [ div
            [ css
                [ width (px 960)
                , margin2 zero auto
                , fontFamilies [ "Helvetica", "Arial", "serif" ]
                ]
            ]
            [ input [ class "search-query", onInput ChangedQuery, value <| model.query, onEnter Search ] []
            , button [ class "search-button", onClick Search ] [ text "Search" ]
            , Html.Styled.map Options (lazy SearchOptions.view model.searchOptions)
            , viewTab model
            ]
        ]
    }


viewTab : Model -> Html Msg
viewTab model =
    div []
        [ div [ css [ displayFlex ] ]
            [ viewTabButton model.tab Main "Top rated"
            , viewTabButton model.tab Favorite "Favorite"
            , viewTabButton model.tab Recommendations "Recommendations"
            ]
        , viewTabData model
        ]


viewTabData : Model -> Html Msg
viewTabData model =
    case ( model.feed, model.genres ) of
        ( Loading, _ ) ->
            text "Loading..."

        ( _, Loading ) ->
            text "Loading..."

        ( Failure feedMsg, _ ) ->
            viewErrorMessage feedMsg

        ( _, Failure genresMsg ) ->
            viewErrorMessage genresMsg

        ( Success _, Success _ ) ->
            div [] <| viewFeed model


viewTabButton : Tab -> Tab -> String -> Html Msg
viewTabButton currentTab tab name =
    let
        active =
            currentTab == tab
    in
    div
        [ css
            [ padding2 (px 8) (px 24)
            , margin (px 8)
            , if active then
                borderBottom3 (px 2) solid (hex "000000")

              else
                border zero
            , cursor pointer
            ]
        , onClick (ChangedTab tab)
        ]
        [ text name ]


viewErrorMessage : String -> Html Msg
viewErrorMessage msg =
    div [ class "error" ] [ text msg ]


viewFeed : Model -> List (Html Msg)
viewFeed model =
    case ( model.feed, model.genres ) of
        ( Success feedData, Success genresData ) ->
            [ Html.Styled.Keyed.node
                "div"
                [ class "results" ]
                (List.map (viewKeyedSearchResult genresData (Session.favoriteMovies model.session)) feedData.movies)
            , lazy viewPagination { page = feedData.page, total = feedData.totalPages }
            ]

        ( _, _ ) ->
            []


viewKeyedSearchResult : List Genre -> List MovieId -> PreviewMovie -> ( String, Html Msg )
viewKeyedSearchResult genres favoriteMovies movie =
    ( MovieId.toString <| Movie.id movie
    , lazy5 Movie.view movie genres favoriteMovies SelectedFavoriteMovie RemovedFavoriteMovie
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
subscriptions model =
    onSessionChange (GotSession << Session.updateWithStoredItems model.session)



-- FETCH


fetchFeed : Model -> Int -> Task Http.Error Feed
fetchFeed model page =
    let
        { session, tab, searchOptions } =
            model

        queries =
            List.filter
                (\q ->
                    case q of
                        Just _ ->
                            True

                        Nothing ->
                            False
                )
                [ SearchOptions.regionQueryParam searchOptions
                , SearchOptions.adultQueryParam searchOptions
                , SearchOptions.languageQueryParam searchOptions
                , Session.tokenQueryParam session
                , Just ("query=" ++ model.query)
                , Just ("page=" ++ String.fromInt page)
                ]

        query =
            queries
                |> List.map (Maybe.withDefault "")
                |> List.intersperse "&"
                |> List.foldr (++) ""

        mainUrl =
            if model.query == "" then
                "movie/top_rated?"

            else
                "search/movie?"

        url =
            case tab of
                Main ->
                    baseUrl
                        ++ mainUrl
                        ++ query

                Favorite ->
                    ""

                Recommendations ->
                    ""
    in
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| feedDecoder
        , timeout = Nothing
        }



-- SERIALIZATION


feedDecoder : Decoder Feed
feedDecoder =
    succeed Feed
        |> DP.required "results" (D.list Movie.previewDecoder)
        |> DP.required "page" D.int
        |> DP.required "total_pages" D.int
        |> DP.required "total_results" D.int



-- TRANSFORMATION


mapSearchOptions : SearchOptions.Options -> Model -> Model
mapSearchOptions searchOptions model =
    { model | searchOptions = searchOptions }



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
