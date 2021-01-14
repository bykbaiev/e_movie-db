module Genre exposing (Genre, decoder, fetch, viewList)

import Api exposing (baseUrl)
import Css exposing (..)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Http
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline as DPipeline
import RequestHelpers exposing (handleJsonResponse)
import Session exposing (Session)
import Task exposing (Task)



-- TYPES


type alias Genre =
    { id : Int
    , name : String
    }



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


fetch : Session -> Task Http.Error (List Genre)
fetch session =
    let
        url =
            baseUrl
                ++ "genre/movie/list?language=en&"
                ++ Maybe.withDefault "" (Session.tokenQueryParam session)
    in
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse genresDecoder
        , timeout = Nothing
        }



-- SERIALIZATION


genresDecoder : Decoder (List Genre)
genresDecoder =
    Json.Decode.succeed identity
        |> DPipeline.required "genres" (Json.Decode.list decoder)


decoder : Decoder Genre
decoder =
    Json.Decode.succeed Genre
        |> DPipeline.required "id" Json.Decode.int
        |> DPipeline.required "name" Json.Decode.string
