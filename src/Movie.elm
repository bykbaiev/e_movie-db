module Movie exposing (PreviewMovie, PreviewMoviesResults, fetch, getPoster, id, view)

import Css exposing (..)
import DateFormat
import Genre exposing (GenresResults)
import Html.Styled exposing (Html, a, button, div, img, p, text)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Http
import Iso8601 exposing (toTime)
import Json.Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline as DPipeline
import RequestHelpers exposing (handleJsonResponse, queryParam)
import SearchOptions exposing (Msg)
import Tab exposing (Tab(..))
import Task exposing (Task)
import Time
import Url exposing (baseUrl, imageUrl)
import ViewHelpers



-- TYPES


type Movie extraInfo
    = Movie Internals extraInfo


type alias Internals =
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


type Preview
    = Preview


type alias PreviewMovie =
    Movie Preview


type alias PreviewMoviesResults =
    { movies : List PreviewMovie
    , page : Int
    , totalPages : Int
    , totalResults : Int
    }



-- VIEW


view : Movie a -> GenresResults -> List Int -> (Int -> msg) -> (Int -> msg) -> Html msg
view movie genres favoriteMovies addToFavorites removeFromFavorites =
    let
        (Movie internals _) =
            movie

        movieGenres =
            List.filter (\genre -> List.member genre.id internals.genreIds) genres

        movieReleaseDate =
            internals.releaseDate
                |> toTime
                |> Result.toMaybe
                |> Maybe.map (DateFormat.format "dd MMM yyyy" Time.utc)
                |> Maybe.withDefault "Unknown"

        isFavorite =
            List.any (\movieId -> movieId == internals.id) favoriteMovies
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
                    [ text internals.title ]
                , if internals.title /= internals.originalTitle then
                    p
                        [ css
                            [ margin2 (px 8) zero
                            , fontStyle italic
                            , color (hex "#999")
                            ]
                        ]
                        [ text internals.originalTitle ]

                  else
                    text ""
                ]
            , Genre.viewList movieGenres
            , div [ css [ textAlign justify ] ]
                [ text <| ViewHelpers.truncateText internals.overview ]
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
                        [ text <| "Rate: " ++ String.fromFloat internals.rate ]
                    , div []
                        [ text <| "Release date: " ++ movieReleaseDate ]
                    ]
                , if isFavorite then
                    viewRemoveFromFavoriteButton internals.id removeFromFavorites

                  else
                    viewAddToFavoriteButton internals.id addToFavorites
                ]
            ]
        ]


viewAddToFavoriteButton : Int -> (Int -> msg) -> Html msg
viewAddToFavoriteButton movieId toMsg =
    button [ onClick <| toMsg movieId ] [ text "Add to favorites" ]


viewRemoveFromFavoriteButton : Int -> (Int -> msg) -> Html msg
viewRemoveFromFavoriteButton movieId toMsg =
    button [ onClick <| toMsg movieId ] [ text "Remove from favorites" ]



-- FETCH MOVIES


fetch : { tab : Tab, token : Maybe String, query : String, options : SearchOptions.Options, page : Int } -> Task Http.Error PreviewMoviesResults
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
            , resolver = Http.stringResolver <| handleJsonResponse <| previewMoviesDecoder
            , timeout = Nothing
            }



-- SERIALIZATION


previewMoviesDecoder : Decoder PreviewMoviesResults
previewMoviesDecoder =
    succeed PreviewMoviesResults
        |> DPipeline.required "results" (Json.Decode.list previewMovieDecoder)
        |> DPipeline.required "page" Json.Decode.int
        |> DPipeline.required "total_pages" Json.Decode.int
        |> DPipeline.required "total_results" Json.Decode.int


movieDecoder : (Internals -> Movie a) -> Decoder (Movie a)
movieDecoder mapper =
    Json.Decode.map mapper internalsDecoder


previewMovieDecoder : Decoder (Movie Preview)
previewMovieDecoder =
    movieDecoder (\internals -> Movie internals Preview)


internalsDecoder : Decoder Internals
internalsDecoder =
    succeed Internals
        |> DPipeline.required "id" Json.Decode.int
        |> DPipeline.required "title" Json.Decode.string
        |> DPipeline.required "vote_average" Json.Decode.float
        |> DPipeline.required "genre_ids" (Json.Decode.list Json.Decode.int)
        |> DPipeline.required "original_language" Json.Decode.string
        |> DPipeline.required "original_title" Json.Decode.string
        |> DPipeline.required "overview" Json.Decode.string
        |> DPipeline.required "poster_path" (Json.Decode.nullable Json.Decode.string)
        |> DPipeline.optional "release_date" Json.Decode.string "Unknown"
        |> DPipeline.required "backdrop_path" (Json.Decode.nullable Json.Decode.string)
        |> DPipeline.required "adult" Json.Decode.bool



-- GETTERS


mapSrc : Maybe String -> String
mapSrc =
    Maybe.withDefault "" << Maybe.map (\src -> imageUrl ++ src)


getPoster : Movie a -> String
getPoster (Movie internals _) =
    let
        poster =
            mapSrc internals.posterPath

        backdrop =
            mapSrc internals.backdropPath

        defaultImg =
            ""
    in
    [ poster, backdrop, defaultImg ]
        |> List.filter (\src -> src /= "")
        |> List.head
        |> Maybe.withDefault defaultImg


id : Movie a -> Int
id (Movie internals _) =
    internals.id
