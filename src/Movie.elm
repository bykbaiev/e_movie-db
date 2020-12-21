module Movie exposing (PreviewMovie, id, previewDecoder, view)

import Api
import Css exposing (..)
import DateFormat
import Genre exposing (GenresResults)
import Html.Styled exposing (Html, a, button, div, img, p, text)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Iso8601 exposing (toTime)
import Json.Decode as D exposing (Decoder, succeed)
import Json.Decode.Pipeline as DP
import MovieId exposing (MovieId)
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



-- VIEW


view : Movie a -> GenresResults -> List MovieId -> (MovieId -> msg) -> (MovieId -> msg) -> Html msg
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


viewAddToFavoriteButton : MovieId -> (MovieId -> msg) -> Html msg
viewAddToFavoriteButton movieId toMsg =
    button [ onClick <| toMsg movieId ] [ text "Add to favorites" ]


viewRemoveFromFavoriteButton : MovieId -> (MovieId -> msg) -> Html msg
viewRemoveFromFavoriteButton movieId toMsg =
    button [ onClick <| toMsg movieId ] [ text "Remove from favorites" ]



-- SERIALIZATION


decoder : (Internals -> Movie a) -> Decoder (Movie a)
decoder mapper =
    D.map mapper internalsDecoder


previewDecoder : Decoder (Movie Preview)
previewDecoder =
    decoder (\internals -> Movie internals Preview)


internalsDecoder : Decoder Internals
internalsDecoder =
    succeed Internals
        |> DP.required "id" MovieId.decoder
        |> DP.required "title" D.string
        |> DP.required "vote_average" D.float
        |> DP.required "genre_ids" (D.list D.int)
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



--(\src -> Api.imageUrl ++ src)


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
