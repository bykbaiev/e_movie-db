module Page.MovieDetails exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Css exposing (..)
import Genre exposing (Genre)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (class, css)
import Html.Styled.Keyed
import Html.Styled.Lazy exposing (lazy5)
import Http
import Loader
import Movie exposing (Feed, FullMovie, PreviewMovie)
import MovieId exposing (MovieId)
import Ports exposing (onSessionChange, storeSession)
import RequestHelpers
import Session exposing (Session)
import StyledDocument exposing (StyledDocument)
import Task



-- TYPES


type Status a
    = Loading
    | LoadingSlowly
    | Success a
    | Failure String



-- MODEL


type alias Model =
    { session : Session
    , id : MovieId
    , details : Status FullMovie
    , recommendations : Status Feed
    , genres : Status (List Genre)
    }


init : Session -> MovieId -> ( Model, Cmd Msg )
init session id =
    let
        model =
            { session = session
            , id = id
            , details = Loading
            , recommendations = Loading
            , genres = Loading
            }
    in
    ( model
    , Cmd.batch
        [ Task.attempt GotGenres <| Genre.fetch session
        , Task.attempt GotDetails <| Movie.fetch session id
        , Task.attempt GotRecommendations <| Movie.fetchRecommendations session id
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loader.slowThreshold
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    onSessionChange (GotSession << Session.updateWithStoredItems model.session)



-- UPDATE


type Msg
    = GotSession Session
    | GotDetails (Result Http.Error FullMovie)
    | GotGenres (Result Http.Error (List Genre))
    | GotRecommendations (Result Http.Error Feed)
    | SelectedFavoriteMovie MovieId
    | RemovedFavoriteMovie MovieId
    | PassedSlowLoadThreshold


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }, Cmd.none )

        GotGenres genresResults ->
            case genresResults of
                Err error ->
                    ( { model | genres = Failure <| RequestHelpers.toString error }, Cmd.none )

                Ok genres ->
                    ( { model | genres = Success genres }, Cmd.none )

        GotDetails detailsResults ->
            case detailsResults of
                Err error ->
                    ( { model | details = Failure <| RequestHelpers.toString error }, Cmd.none )

                Ok details ->
                    ( { model | details = Success details }, Cmd.none )

        SelectedFavoriteMovie id ->
            let
                favoriteMovies =
                    id :: Session.favoriteMovies model.session
            in
            ( model, storeFavorite model.session favoriteMovies )

        RemovedFavoriteMovie id ->
            let
                favoriteMovies =
                    List.filter (\movieId -> movieId /= id) (Session.favoriteMovies model.session)
            in
            ( model, storeFavorite model.session favoriteMovies )

        GotRecommendations recommendationResults ->
            case recommendationResults of
                Err error ->
                    ( { model | recommendations = Failure <| RequestHelpers.toString error }, Cmd.none )

                Ok recommendations ->
                    ( { model | recommendations = Success recommendations }, Cmd.none )

        PassedSlowLoadThreshold ->
            let
                details =
                    case model.details of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other

                genres =
                    case model.genres of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other

                recommendations =
                    case model.recommendations of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
            ( { model
                | details = details
                , recommendations = recommendations
                , genres = genres
              }
            , Cmd.none
            )


storeFavorite : Session -> List MovieId -> Cmd Msg
storeFavorite session fMovies =
    let
        sessionValue =
            Session.encode session fMovies
    in
    storeSession sessionValue



-- VIEW


view : Model -> StyledDocument Msg
view model =
    { title = "Movie " ++ MovieId.toString model.id
    , body =
        [ div
            [ css
                [ position relative
                , paddingBottom <| px 32
                , width (px 960)
                , margin2 zero auto
                , fontFamilies [ "Helvetica", "Arial", "serif" ]
                ]
            ]
            [ viewMovie model
            , viewRecommendations model
            ]
        ]
    }


viewMovie : Model -> Html Msg
viewMovie model =
    case model.details of
        Loading ->
            text ""

        LoadingSlowly ->
            Loader.view

        Success movie ->
            Movie.view movie (Session.favoriteMovies model.session) SelectedFavoriteMovie RemovedFavoriteMovie

        Failure err ->
            viewErrorMessage err


viewRecommendations : Model -> Html Msg
viewRecommendations model =
    case ( model.recommendations, model.genres ) of
        ( Failure err, _ ) ->
            viewErrorMessage err

        ( _, Failure err ) ->
            viewErrorMessage err

        ( LoadingSlowly, _ ) ->
            Loader.view

        ( _, LoadingSlowly ) ->
            Loader.view

        ( Loading, _ ) ->
            text ""

        ( _, Loading ) ->
            text ""

        ( Success recommendations, Success genres ) ->
            case recommendations.movies of
                [] ->
                    div [] [ text "There are no any recommendations" ]

                movies ->
                    Html.Styled.Keyed.node
                        "div"
                        [ class "results" ]
                        (List.map (viewKeyedRecommendation genres (Session.favoriteMovies model.session)) <| List.take 3 movies)


viewKeyedRecommendation : List Genre -> List MovieId -> PreviewMovie -> ( String, Html Msg )
viewKeyedRecommendation genres favoriteMovies movie =
    ( MovieId.toString <| Movie.id movie
    , lazy5 Movie.viewPreview movie genres favoriteMovies SelectedFavoriteMovie RemovedFavoriteMovie
    )


viewErrorMessage : String -> Html Msg
viewErrorMessage msg =
    div [ class "error" ] [ text msg ]
