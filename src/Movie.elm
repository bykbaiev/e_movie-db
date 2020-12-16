module Movie exposing (Movie, MoviesResults, fetch, getPoster, view)

import Css exposing (..)
import DateFormat
import Genre exposing (GenresResults)
import Html.Styled exposing (Html, a, div, img, p, text)
import Html.Styled.Attributes exposing (css, href, src)
import Http
import Iso8601 exposing (toTime)
import Json.Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline as DPipeline
import RequestHelpers exposing (handleJsonResponse, queryParam)
import SearchOptions
import Tab exposing (Tab(..))
import Task exposing (Task)
import Time
import Url exposing (baseUrl, imageUrl)
import ViewHelpers



-- TYPES


type alias Movie =
    { id : Int
    , title : String
    , rate : Float
    , genreIds : List Int
    , originalLanguage : String
    , originalTitle : String
    , overview : String
    , posterPath : Maybe String
    , releaseDate : String
    , backdropPath : Maybe String
    , adult : Bool
    }


type alias MoviesResults =
    { movies : List Movie
    , page : Int
    , totalPages : Int
    , totalResults : Int
    }



-- VIEW


view : Movie -> GenresResults -> Html msg
view movie genres =
    let
        movieGenres =
            List.filter (\genre -> List.member genre.id movie.genreIds) genres

        movieReleaseDate =
            movie.releaseDate
                |> toTime
                |> Result.toMaybe
                |> Maybe.map (DateFormat.format "dd MMM yyyy" Time.utc)
                |> Maybe.withDefault "Unknown"
    in
    div
        [ css
            [ displayFlex
            , justifyContent spaceBetween
            , margin2 (px 8) zero
            , padding (px 16)
            , height (px 332)
            , boxShadow4 zero (px 4) (px 5) (hex "#eee")
            ]
        ]
        [ div
            [ css
                [ width (pct 35)
                , overflow hidden
                ]
            ]
            [ img
                [ src <| getPoster movie
                , css
                    [ display block
                    , margin2 zero auto
                    , height (px 300)
                    ]
                ]
                []
            ]
        , div
            [ css
                [ position relative
                , width (pct 65)
                ]
            ]
            [ div []
                [ a
                    [ href "#"
                    , css
                        [ margin2 (px 8) zero
                        , fontSize (px 18)
                        ]
                    ]
                    [ text movie.title ]
                , if movie.title /= movie.originalTitle then
                    p
                        [ css
                            [ margin2 (px 8) zero
                            , fontStyle italic
                            , color (hex "#999")
                            ]
                        ]
                        [ text movie.originalTitle ]

                  else
                    text ""
                ]
            , Genre.viewList movieGenres
            , div [ css [ textAlign justify ] ]
                [ text <| ViewHelpers.truncateText movie.overview ]
            , div
                [ css
                    [ displayFlex
                    , justifyContent spaceBetween
                    , alignItems center
                    , position absolute
                    , right zero
                    , bottom zero
                    , left zero
                    ]
                ]
                [ div
                    [ css
                        [ displayFlex
                        , alignItems center
                        ]
                    ]
                    [ div
                        [ css [ marginRight (px 8) ] ]
                        [ text <| "Rate: " ++ String.fromFloat movie.rate ]
                    , div []
                        [ text <| "Release date: " ++ movieReleaseDate ]
                    ]
                , div [] [ text "Is favorite?" ]
                ]
            ]
        ]



-- FETCH MOVIES


fetch : { tab : Tab, token : Maybe String, query : String, options : SearchOptions.Options, page : Int } -> Task Http.Error MoviesResults
fetch { tab, token, query, options, page } =
    let
        isMissingToken =
            token
                |> Maybe.map (\t -> t == "")
                |> Maybe.withDefault True

        tokenQuery =
            queryParam
                { name = "api_key"
                , value = Maybe.withDefault "" token
                , isFirst = True
                }

        searchQuery =
            queryParam
                { name = "query"
                , value = query
                , isFirst = False
                }

        regionQuery =
            queryParam
                { name = "region"
                , value = Maybe.withDefault "" options.region
                , isFirst = False
                }

        languageQuery =
            queryParam
                { name = "language"
                , value = Maybe.withDefault "" options.language
                , isFirst = False
                }

        pageQuery =
            queryParam
                { name = "page"
                , value = String.fromInt page
                , isFirst = False
                }

        url =
            case tab of
                Main ->
                    if query /= "" then
                        baseUrl
                            ++ "search/movie"
                            ++ tokenQuery
                            ++ searchQuery
                            ++ regionQuery
                            ++ languageQuery
                            ++ pageQuery

                    else
                        baseUrl
                            ++ "movie/top_rated"
                            ++ tokenQuery
                            ++ regionQuery
                            ++ languageQuery
                            ++ pageQuery

                Favorite ->
                    ""

                Recommendations ->
                    ""
    in
    if isMissingToken then
        Task.fail <| Http.BadUrl "Missing API token"

    else
        Http.task
            { method = "GET"
            , headers = []
            , url = url
            , body = Http.emptyBody
            , resolver = Http.stringResolver <| handleJsonResponse <| moviesDecoder
            , timeout = Nothing
            }



-- SERIALIZATION


moviesDecoder : Decoder MoviesResults
moviesDecoder =
    succeed MoviesResults
        |> DPipeline.required "results" (Json.Decode.list movieDecoder)
        |> DPipeline.required "page" Json.Decode.int
        |> DPipeline.required "total_pages" Json.Decode.int
        |> DPipeline.required "total_results" Json.Decode.int


movieDecoder : Decoder Movie
movieDecoder =
    succeed Movie
        |> DPipeline.required "id" Json.Decode.int
        |> DPipeline.required "title" Json.Decode.string
        |> DPipeline.required "vote_average" Json.Decode.float
        |> DPipeline.required "genre_ids" (Json.Decode.list Json.Decode.int)
        |> DPipeline.required "original_language" Json.Decode.string
        |> DPipeline.required "original_title" Json.Decode.string
        |> DPipeline.required "overview" Json.Decode.string
        |> DPipeline.required "poster_path" (Json.Decode.nullable Json.Decode.string)
        |> DPipeline.required "release_date" Json.Decode.string
        |> DPipeline.required "backdrop_path" (Json.Decode.nullable Json.Decode.string)
        |> DPipeline.required "adult" Json.Decode.bool



-- GETTERS


mapSrc : Maybe String -> String
mapSrc =
    Maybe.withDefault "" << Maybe.map (\src -> imageUrl ++ src)


getPoster : Movie -> String
getPoster movie =
    let
        poster =
            mapSrc movie.posterPath

        backdrop =
            mapSrc movie.backdropPath

        defaultImg =
            ""
    in
    [ poster, backdrop, defaultImg ]
        |> List.filter (\src -> src /= "")
        |> List.head
        |> Maybe.withDefault defaultImg
