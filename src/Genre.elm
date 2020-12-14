module Genre exposing (Genre, GenresResults, fetch)

import Http
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import RequestHelpers exposing (handleJsonResponse, queryParam)
import Task exposing (Task)
import Url exposing (baseUrl)



-- TYPES


type alias Genre =
    { id : Int
    , name : String
    }


type alias GenresResults =
    List Genre



-- FETCH GENRES


fetch : Maybe String -> Task Http.Error GenresResults
fetch token =
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

        url =
            baseUrl ++ "genre/movie/list" ++ tokenQuery ++ "&language=en"
    in
    if isMissingToken then
        Task.fail <| Http.BadUrl "Missing API token"

    else
        Http.task
            { method = "GET"
            , headers = []
            , url = url
            , body = Http.emptyBody
            , resolver = Http.stringResolver <| handleJsonResponse genresDecoder
            , timeout = Nothing
            }



-- SERIALIZATION


genresDecoder : Decoder GenresResults
genresDecoder =
    Json.Decode.succeed identity
        |> required "genres" (Json.Decode.list genreDecoder)


genreDecoder : Decoder Genre
genreDecoder =
    Json.Decode.succeed Genre
        |> required "id" Json.Decode.int
        |> required "name" Json.Decode.string
