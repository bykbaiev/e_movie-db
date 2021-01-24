module Page.Home exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Api exposing (baseUrl)
import Color
import Css exposing (..)
import Css.Transitions as Transitions exposing (transition)
import Genre exposing (Genre)
import Html.Styled exposing (Html, a, button, div, input, text)
import Html.Styled.Attributes exposing (class, css, value)
import Html.Styled.Events exposing (keyCode, on, onClick, onInput)
import Html.Styled.Keyed
import Html.Styled.Lazy exposing (lazy, lazy5)
import Http
import Json.Decode as D
import Loader
import Material.Icons.Action exposing (search)
import Material.Icons.Navigation exposing (close)
import Movie exposing (Feed, PreviewMovie)
import MovieId exposing (MovieId)
import Ports exposing (onSessionChange, storeSession)
import Regex exposing (Options)
import RequestHelpers
import SearchOptions exposing (updateOptions)
import Session exposing (Session, favoriteMovies)
import String
import StyledDocument exposing (StyledDocument)
import Svg.Styled exposing (svg)
import Svg.Styled.Attributes as SvgAttr
import Task exposing (Task)
import UI.IconButton exposing (iconButton)



-- TYPES


type Tab
    = Main
    | Favorite
    | Recommendations


type Status a
    = Loading
    | LoadingSlowly
    | Success a
    | Failure String


type FeedLoadingPayload
    = MainFeedLoadingPayload
    | FavoriteFeedLoadingPayload (List PreviewMovie)
    | RecommendationsFeedLoadingPayload (List PreviewMovie) Int


type FeedStatus
    = FeedLoading FeedLoadingPayload
    | FeedLoadingSlowly FeedLoadingPayload
    | FeedSuccess Feed
    | FeedFailure String



-- MODEL


type alias Model =
    { session : Session
    , query : String
    , searchOptions : SearchOptions.Options -- TODO move logic here, make SearchOptions simple module without TEA
    , tab : Tab
    , feed : FeedStatus
    , genres : Status (List Genre)
    , searchModalOpened : Bool
    }


init : Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session
            , query = ""
            , searchOptions = SearchOptions.initialModel
            , tab = Main
            , feed = FeedLoading MainFeedLoadingPayload
            , genres = Loading
            , searchModalOpened = False
            }
    in
    ( model
    , Cmd.batch
        [ fetchFeed model 1
        , Task.attempt GotGenres <| Genre.fetch session
        , Task.perform (always PassedSlowLoadThreshold) Loader.slowThreshold
        ]
    )



-- UPDATE


type Msg
    = ChangedQuery String
    | ClearedSearchQuery
    | Search
    | GotFeed (Result Http.Error Feed)
    | GotInBetweenFeedMovies Tab (Result Http.Error (List PreviewMovie))
    | PartialSuccess (Model -> ( Model, Cmd Msg ))
    | GotGenres (Result Http.Error (List Genre))
    | Options SearchOptions.Msg
    | ChangedPage Int
    | ChangedTab Tab
    | SelectedFavoriteMovie MovieId
    | GotSession Session
    | RemovedFavoriteMovie MovieId
    | PassedSlowLoadThreshold
    | OpenedSearchModal
    | ClosedSearchModal
    | NoMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search ->
            ( { model
                | query = ""
                , feed = FeedLoading MainFeedLoadingPayload
              }
            , Cmd.batch
                [ Task.perform (always ClosedSearchModal) <| Task.succeed Nothing
                , fetchFeed model 1
                , Task.perform (always PassedSlowLoadThreshold) Loader.slowThreshold
                ]
            )

        ChangedQuery query ->
            ( { model | query = query }, Cmd.none )

        ClearedSearchQuery ->
            ( { model | query = "" }, Task.perform (always ClosedSearchModal) <| Task.succeed Nothing )

        Options searchOptionsMsg ->
            let
                ( searchOptions, shouldReload ) =
                    updateOptions searchOptionsMsg model.searchOptions

                updatedModel =
                    if shouldReload then
                        { model | searchOptions = searchOptions, feed = FeedLoading MainFeedLoadingPayload }

                    else
                        { model | searchOptions = searchOptions }

                cmd =
                    if shouldReload then
                        Cmd.batch
                            [ fetchFeed updatedModel 1
                            , Task.perform (always PassedSlowLoadThreshold) Loader.slowThreshold
                            ]

                    else
                        Cmd.none
            in
            ( updatedModel, cmd )

        ChangedPage page ->
            ( { model | feed = FeedLoading MainFeedLoadingPayload }
            , Cmd.batch
                [ fetchFeed model page
                , Task.perform (always PassedSlowLoadThreshold) Loader.slowThreshold
                ]
            )

        GotFeed feedResults ->
            case feedResults of
                Err error ->
                    ( { model | feed = FeedFailure <| RequestHelpers.toString error }, Cmd.none )

                Ok feed ->
                    ( { model | feed = FeedSuccess feed }, Cmd.none )

        GotInBetweenFeedMovies tab movieResults ->
            if tab == model.tab then
                ( withInBetweenFeedMovies movieResults model, Cmd.none )

            else
                ( model, Cmd.none )

        PartialSuccess updater ->
            updater model

        ChangedTab tab ->
            let
                updatedModel =
                    { model | tab = tab, feed = FeedLoading <| getLoadingPayloadForTab tab }
            in
            ( updatedModel
            , Cmd.batch
                [ fetchFeed updatedModel 1
                , Task.perform (always PassedSlowLoadThreshold) Loader.slowThreshold
                ]
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

        PassedSlowLoadThreshold ->
            let
                feed =
                    case model.feed of
                        FeedLoading movies ->
                            FeedLoadingSlowly movies

                        other ->
                            other

                genres =
                    case model.genres of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
            ( { model | feed = feed, genres = genres }, Cmd.none )

        OpenedSearchModal ->
            ( { model | searchModalOpened = True }, Cmd.none )

        ClosedSearchModal ->
            ( { model | searchModalOpened = False }, Cmd.none )

        NoMsg ->
            ( model, Cmd.none )


storeFavorite : Session -> List MovieId -> Cmd Msg
storeFavorite session fMovies =
    let
        sessionValue =
            Session.encode session fMovies
    in
    storeSession sessionValue


getLoadingPayloadForTab : Tab -> FeedLoadingPayload
getLoadingPayloadForTab tab =
    case tab of
        Main ->
            MainFeedLoadingPayload

        Favorite ->
            FavoriteFeedLoadingPayload []

        Recommendations ->
            RecommendationsFeedLoadingPayload [] 0



-- VIEW


view : Model -> StyledDocument Msg
view model =
    { title = "Home page - MovieDB"
    , body =
        [ div
            [ css <|
                if model.searchModalOpened then
                    [ overflow hidden, height <| calc (vh 100) minus (px 200) ]

                else
                    []
            ]
            [ if model.searchModalOpened then
                viewSearchModal model

              else
                text ""
            , div
                [ css
                    [ width (px 960)
                    , margin2 zero auto
                    , fontFamilies [ "Helvetica", "Arial", "serif" ]
                    ]
                ]
                [ input [ class "search-query", onInput ChangedQuery, value <| model.query, onEnter Search ] []
                , button [ class "search-button", onClick Search ] [ text "Search" ]
                , Html.Styled.map Options (lazy SearchOptions.view model.searchOptions)
                , viewTabs model
                ]
            ]
        ]
    }


viewSearchModal : Model -> Html Msg
viewSearchModal model =
    div
        [ css
            [ position fixed
            , top zero
            , right zero
            , bottom zero
            , left zero
            , backgroundColor <| rgba 255 255 255 0.9
            , height <| pct 100
            , zIndex <| int 1
            ]
        ]
        [ iconButton
            (Just
                [ position absolute
                , top <| px 16
                , right <| px 16
                , backgroundColor transparent
                ]
            )
            ClosedSearchModal
            24
            (close Color.blue 24)
        , input
            [ css
                [ position absolute
                , top <| pct 50
                , left <| pct 50
                , fontSize <| px 24
                , transform <| translate <| pct -50
                ]
            , onInput ChangedQuery
            , onEsc ClearedSearchQuery
            , value <| model.query
            , onEnter Search
            ]
            []
        ]


viewTabs : Model -> Html Msg
viewTabs model =
    div []
        [ div
            [ css
                [ displayFlex
                , justifyContent spaceBetween
                , alignItems center
                ]
            ]
            [ div [ css [ displayFlex ] ]
                [ viewTabButton model.tab Main "Top rated"
                , viewTabButton model.tab Favorite "Favorite"
                , viewTabButton model.tab Recommendations "Recommendations"
                ]
            , iconButton Nothing OpenedSearchModal 24 <| search Color.blue 24
            ]
        , viewTab model
        ]


viewTab : Model -> Html Msg
viewTab model =
    case ( model.feed, model.genres ) of
        ( FeedLoadingSlowly _, _ ) ->
            Loader.view

        ( _, LoadingSlowly ) ->
            Loader.view

        ( FeedFailure feedMsg, _ ) ->
            viewErrorMessage feedMsg

        ( _, Failure genresMsg ) ->
            viewErrorMessage genresMsg

        ( FeedSuccess _, Success _ ) ->
            div [] <| viewFeed model

        ( _, _ ) ->
            div [] []


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
        ( FeedSuccess feedData, Success genresData ) ->
            case feedData.movies of
                [] ->
                    [ div [] [ text "There are no any movies" ] ]

                _ ->
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
    , lazy5 Movie.viewPreview movie genres favoriteMovies SelectedFavoriteMovie RemovedFavoriteMovie
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
    if total < 2 then
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
        , onClick <|
            if selected then
                NoMsg

            else
                ChangedPage page
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


fetchFeed : Model -> Int -> Cmd Msg
fetchFeed model page =
    case model.tab of
        Main ->
            Task.attempt GotFeed <| fetchMainFeed model page

        Favorite ->
            fetchFavoriteMovies model

        Recommendations ->
            fetchRecommendationMovies model


fetchMainFeed : Model -> Int -> Task Http.Error Feed
fetchMainFeed model page =
    let
        { session, searchOptions } =
            model

        queries =
            List.filter
                ((/=) Nothing)
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
            baseUrl ++ mainUrl ++ query
    in
    Movie.fetchList url


fetchFavoriteMovies : Model -> Cmd Msg
fetchFavoriteMovies model =
    let
        ids =
            Session.favoriteMovies model.session
    in
    case List.map (Movie.fetchPreview model.session) ids of
        [] ->
            Task.attempt GotFeed <| Task.succeed (Feed [] 1 1 0)

        requests ->
            parallelize (List.map (Task.map List.singleton) requests)


fetchRecommendationMovies : Model -> Cmd Msg
fetchRecommendationMovies model =
    let
        ids =
            Session.favoriteMovies model.session
    in
    case List.map (Movie.fetchRecommendations model.session) ids of
        [] ->
            Task.attempt GotFeed <| Task.succeed (Feed [] 1 1 0)

        requests ->
            parallelize (List.map (Task.map .movies) requests)


parallelize : List (Task Http.Error (List PreviewMovie)) -> Cmd Msg
parallelize tasks =
    Cmd.batch
        (List.map
            (Task.attempt
                (\result ->
                    case result of
                        Ok values ->
                            PartialSuccess
                                (\model ->
                                    ( model
                                    , case model.feed of
                                        FeedLoading payload ->
                                            updateLoadingFeed payload values model

                                        FeedLoadingSlowly payload ->
                                            updateLoadingFeed payload values model

                                        _ ->
                                            Task.perform (always NoMsg) <| Task.succeed Nothing
                                    )
                                )

                        Err msg ->
                            GotFeed <| Result.Err msg
                )
            )
            tasks
        )


updateLoadingFeed : FeedLoadingPayload -> List PreviewMovie -> Model -> Cmd Msg
updateLoadingFeed payload values model =
    let
        previousValues =
            case payload of
                MainFeedLoadingPayload ->
                    []

                FavoriteFeedLoadingPayload movies ->
                    movies

                RecommendationsFeedLoadingPayload movies _ ->
                    movies

        updatedValues =
            previousValues ++ values

        count =
            List.length updatedValues

        favoriteMoviesCount =
            List.length <| Session.favoriteMovies model.session

        enough =
            case payload of
                MainFeedLoadingPayload ->
                    True

                FavoriteFeedLoadingPayload _ ->
                    favoriteMoviesCount == count

                RecommendationsFeedLoadingPayload _ batchesCount ->
                    favoriteMoviesCount == batchesCount + 1
    in
    if enough then
        Task.attempt GotFeed <| Task.succeed <| Feed updatedValues 1 1 count

    else
        Task.attempt (GotInBetweenFeedMovies model.tab) <| Task.succeed updatedValues



-- TRANSFORMATION


withInBetweenFeedMovies : Result Http.Error (List PreviewMovie) -> Model -> Model
withInBetweenFeedMovies movieResults model =
    case movieResults of
        Err error ->
            case model.feed of
                FeedLoading _ ->
                    { model | feed = FeedFailure <| RequestHelpers.toString error }

                FeedLoadingSlowly _ ->
                    { model | feed = FeedFailure <| RequestHelpers.toString error }

                _ ->
                    model

        Ok movies ->
            let
                getUpdatedPayload payload =
                    case payload of
                        MainFeedLoadingPayload ->
                            MainFeedLoadingPayload

                        FavoriteFeedLoadingPayload _ ->
                            FavoriteFeedLoadingPayload movies

                        RecommendationsFeedLoadingPayload _ count ->
                            RecommendationsFeedLoadingPayload movies (count + 1)
            in
            case model.feed of
                FeedLoading payload ->
                    { model | feed = FeedLoading <| getUpdatedPayload payload }

                FeedLoadingSlowly payload ->
                    { model | feed = FeedLoadingSlowly <| getUpdatedPayload payload }

                _ ->
                    model



-- EVENTS


onEnter : Msg -> Html.Styled.Attribute Msg
onEnter =
    onKeyPress 13


onEsc : Msg -> Html.Styled.Attribute Msg
onEsc =
    onKeyPress 27


onKeyPress : Int -> Msg -> Html.Styled.Attribute Msg
onKeyPress code msg =
    let
        isKey c =
            if c == code then
                D.succeed msg

            else
                D.fail "not ENTER"
    in
    on "keydown" (D.andThen isKey keyCode)



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
