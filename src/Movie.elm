module Movie exposing (Movie, MoviesResults, fetch, getPoster)

import Http
import Json.Decode exposing (Decoder, bool, float, int, list, nullable, string, succeed)
import Json.Decode.Pipeline exposing (required)
import RequestHelpers exposing (handleJsonResponse, queryParam)
import SearchOptions
import Tab exposing (..)
import Task exposing (Task)
import Url exposing (baseUrl, imageUrl)



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
        |> required "results" (list movieDecoder)
        |> required "page" int
        |> required "total_pages" int
        |> required "total_results" int


movieDecoder : Decoder Movie
movieDecoder =
    succeed Movie
        |> required "id" int
        |> required "title" string
        |> required "vote_average" float
        |> required "genre_ids" (list int)
        |> required "original_language" string
        |> required "original_title" string
        |> required "overview" string
        |> required "poster_path" (nullable string)
        |> required "release_date" string
        |> required "backdrop_path" (nullable string)
        |> required "adult" bool



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
