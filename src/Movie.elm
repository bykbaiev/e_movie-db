module Movie exposing
    ( FullMovie
    , PreviewMovie
    , decoder
    , fetch
    , id
    , previewDecoder
    , toPreview
    , view
    , viewPreview
    )

import Api exposing (baseUrl)
import Css exposing (..)
import DateFormat
import Genre exposing (Genre)
import Html.Styled exposing (Html, a, button, div, h2, img, p, text)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Http
import Iso8601 exposing (toTime)
import Json.Decode as D exposing (Decoder, succeed)
import Json.Decode.Pipeline as DP
import MovieId exposing (MovieId)
import RequestHelpers
import Route
import Session exposing (Session)
import Task exposing (Task)
import Time
import ViewHelpers



-- TYPES


type Movie extraInfo
    = Movie Internals extraInfo


type alias Internals =
    { id : MovieId
    , title : String
    , rate : Float
    , originalLanguage : String
    , originalTitle : String
    , overview : String
    , posterPath : Maybe String
    , releaseDate : String
    , backdropPath : Maybe String
    , adult : Bool
    }


type Preview
    = Preview (List Int)


type Full
    = Full FullExtraInfo


type alias FullExtraInfo =
    { genres : List Genre
    , budget : Int
    , homepage : String
    , imdbId : Maybe String
    , productionCountries : List String
    , runtime : Int
    , status : String
    }


type alias PreviewMovie =
    Movie Preview


type alias FullMovie =
    Movie Full



-- VIEW


view : FullMovie -> List MovieId -> (MovieId -> msg) -> (MovieId -> msg) -> Html msg
view movie favoriteMovies addToFavorites removeFromFavorites =
    let
        (Movie internals (Full genres)) =
            movie
    in
    div
        []
        [ h2
            [ css
                [ margin2 (px 8) zero
                , fontSize (px 18)
                ]
            ]
            [ text internals.title ]
        ]


viewPreview : PreviewMovie -> List Genre -> List MovieId -> (MovieId -> msg) -> (MovieId -> msg) -> Html msg
viewPreview movie genres favoriteMovies addToFavorites removeFromFavorites =
    let
        (Movie internals (Preview genreIds)) =
            movie

        movieGenres =
            List.filter (\genre -> List.member genre.id genreIds) genres

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
                [ src <| poster movie
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
                    [ Route.href <| Route.Movie <| id movie
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


viewAddToFavoriteButton : MovieId -> (MovieId -> msg) -> Html msg
viewAddToFavoriteButton movieId toMsg =
    button [ onClick <| toMsg movieId ] [ text "Add to favorites" ]


viewRemoveFromFavoriteButton : MovieId -> (MovieId -> msg) -> Html msg
viewRemoveFromFavoriteButton movieId toMsg =
    button [ onClick <| toMsg movieId ] [ text "Remove from favorites" ]



-- SERIALIZATION


decoder : Decoder (Movie Full)
decoder =
    D.map2
        (\internals extra -> Movie internals <| Full extra)
        fullInternalsDecoder
        fullExtraInfoDecoder


fullExtraInfoDecoder : Decoder FullExtraInfo
fullExtraInfoDecoder =
    succeed FullExtraInfo
        |> DP.required "genres" (D.list Genre.decoder)
        |> DP.required "budget" D.int
        |> DP.required "homepage" D.string
        |> DP.required "imdb_id" (D.nullable D.string)
        |> DP.required "production_countries" (D.list <| D.field "name" D.string)
        |> DP.required "runtime" D.int
        |> DP.required "status" D.string


previewDecoder : Decoder (Movie Preview)
previewDecoder =
    D.map2
        (\internals genreIds -> Movie internals <| Preview genreIds)
        previewInternalsDecoder
        (D.field "genre_ids" <| D.list D.int)


previewInternalsDecoder : Decoder Internals
previewInternalsDecoder =
    succeed Internals
        |> DP.required "id" MovieId.decoder
        |> DP.required "title" D.string
        |> DP.required "vote_average" D.float
        |> DP.required "original_language" D.string
        |> DP.required "original_title" D.string
        |> DP.required "overview" D.string
        |> DP.required "poster_path" (D.nullable D.string)
        |> DP.optional "release_date" D.string "Unknown"
        |> DP.required "backdrop_path" (D.nullable D.string)
        |> DP.required "adult" D.bool


fullInternalsDecoder : Decoder Internals
fullInternalsDecoder =
    succeed Internals
        |> DP.required "id" MovieId.decoder
        |> DP.required "title" D.string
        |> DP.required "vote_average" D.float
        |> DP.required "original_language" D.string
        |> DP.required "original_title" D.string
        |> DP.required "overview" D.string
        |> DP.required "poster_path" (D.nullable D.string)
        |> DP.optional "release_date" D.string "Unknown"
        |> DP.required "backdrop_path" (D.nullable D.string)
        |> DP.required "adult" D.bool



-- GETTERS


mapSrc : Maybe String -> String
mapSrc =
    Maybe.withDefault "" << Maybe.map ((++) Api.imageUrl)


poster : Movie a -> String
poster (Movie internals _) =
    let
        moviePoster =
            mapSrc internals.posterPath

        backdrop =
            mapSrc internals.backdropPath

        defaultImg =
            ""
    in
    [ moviePoster, backdrop, defaultImg ]
        |> List.filter (\src -> src /= "")
        |> List.head
        |> Maybe.withDefault defaultImg


id : Movie a -> MovieId
id (Movie internals _) =
    internals.id



-- FETCH


fetch : Session -> MovieId -> Task Http.Error FullMovie
fetch session movieId =
    let
        query =
            Maybe.withDefault "" <| Session.tokenQueryParam session

        url =
            baseUrl ++ "movie/" ++ MovieId.toString movieId ++ "?" ++ query
    in
    RequestHelpers.fetch url decoder



-- TRANSFORMATIONS


toPreview : FullMovie -> PreviewMovie
toPreview (Movie internals (Full { genres })) =
    Movie internals (Preview <| List.map .id genres)
