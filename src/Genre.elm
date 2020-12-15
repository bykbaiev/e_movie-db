module Genre exposing (Genre, GenresResults, fetch, viewList)

import Css exposing (..)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Http
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline as DPipeline
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



-- VIEW


viewList : List Genre -> Html msg
viewList genres =
    div
        [ css
            [ displayFlex
            , margin2 (px 8) zero
            ]
        ]
        (List.map viewGenreChip genres)


viewGenreChip : Genre -> Html msg
viewGenreChip genre =
    div
        [ css
            [ padding2 (px 4) (px 8)
            , margin4 zero (px 4) zero zero
            , borderRadius (px 5)
            , backgroundColor (hex "#eee")
            ]
        ]
        [ text genre.name ]



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
        |> DPipeline.required "genres" (Json.Decode.list genreDecoder)


genreDecoder : Decoder Genre
genreDecoder =
    Json.Decode.succeed Genre
        |> DPipeline.required "id" Json.Decode.int
        |> DPipeline.required "name" Json.Decode.string
