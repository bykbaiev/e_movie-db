module Movie exposing (Movie, MoviesResults, fetch)

import Http
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import RequestHelpers exposing (handleJsonResponse, queryParam)
import SearchOptions
import Tab exposing (..)
import Task exposing (Task)
import Url exposing (baseUrl)



-- TYPES


type alias Movie =
    { id : Int
    , title : String
    , rate : Float
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
    Json.Decode.succeed MoviesResults
        |> required "results" (Json.Decode.list movieDecoder)
        |> required "page" Json.Decode.int
        |> required "total_pages" Json.Decode.int
        |> required "total_results" Json.Decode.int


movieDecoder : Decoder Movie
movieDecoder =
    Json.Decode.succeed Movie
        |> required "id" Json.Decode.int
        |> required "title" Json.Decode.string
        |> required "vote_average" Json.Decode.float
