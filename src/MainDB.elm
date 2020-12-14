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
import Movie exposing (Movie, MoviesResults)
import Ports exposing (storeQuery)
import SearchOptions exposing (updateOptions)
import String
import Tab exposing (..)
import Task



-- TYPES


type alias Flags =
    { query : Maybe String
    , apiToken : Maybe String
    }


type alias Model =
    { query : String
    , results : MoviesResults
    , errorMessage : Maybe String
    , apiToken : Maybe String
    , searchOptions : SearchOptions.Options
    , tab : Tab
    }


type alias Genre =
    { id : Int
    , name : String
    }


type Msg
    = SetQuery String
    | Search
    | GotMovies (Result Http.Error MoviesResults)
    | Options SearchOptions.Msg
    | SetPage Int
    | SetTab Tab



-- MODEL


initialModel : Model
initialModel =
    { query = ""
    , results =
        { movies = []
        , page = 1
        , totalResults = 0
        , totalPages = 0
        }
    , errorMessage = Nothing
    , apiToken = Nothing
    , searchOptions = SearchOptions.initialModel
    , tab = Main
    }


init : Flags -> ( Model, Cmd Msg )
init { query, apiToken } =
    let
        initialQuery =
            Maybe.withDefault "" query
    in
    ( { initialModel
        | query = initialQuery
        , apiToken = apiToken
      }
    , { tab = initialModel.tab
      , token = apiToken
      , query = initialQuery
      , options = initialModel.searchOptions
      , page = initialModel.results.page
      }
        |> Movie.fetch
        |> Task.attempt GotMovies
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search ->
            ( { model | errorMessage = Nothing }
            , { query = model.query
              , token = model.apiToken
              , options = model.searchOptions
              , page = 1
              , tab = model.tab
              }
                |> Movie.fetch
                |> Task.attempt GotMovies
            )

        SetQuery query ->
            ( { model | query = query }, storeQuery query )

        Options searchOptionsMsg ->
            let
                ( searchOptions, shouldReload ) =
                    updateOptions searchOptionsMsg model.searchOptions

                cmd =
                    if shouldReload then
                        { query = model.query
                        , token = model.apiToken
                        , options = searchOptions
                        , page = 1
                        , tab = model.tab
                        }
                            |> Movie.fetch
                            |> Task.attempt GotMovies

                    else
                        Cmd.none
            in
            ( { model | searchOptions = searchOptions }
            , cmd
            )

        SetPage page ->
            ( model
            , { query = model.query
              , token = model.apiToken
              , options = model.searchOptions
              , page = page
              , tab = model.tab
              }
                |> Movie.fetch
                |> Task.attempt GotMovies
            )

        GotMovies moviesResults ->
            case moviesResults of
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

        SetTab tab ->
            ( { model | tab = tab }, Cmd.none )



-- VIEW


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



-- EVENTS


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



-- SERIALIZATION


genreDecoder : Decoder Genre
genreDecoder =
    Json.Decode.succeed Genre
        |> DPipeline.required "id" Json.Decode.int
        |> DPipeline.required "name" Json.Decode.string



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
