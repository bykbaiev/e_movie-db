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
import Http
import Movie exposing (FullMovie)
import MovieId exposing (MovieId)
import Ports exposing (onSessionChange, storeSession)
import RequestHelpers
import Session exposing (Session)
import StyledDocument exposing (StyledDocument)
import Task



-- TYPES


type Status a
    = Loading
    | Success a
    | Failure String


type RecommendationStatus
    = RecommendationsLoading (Maybe (List FullMovie))
    | RecommendationsSuccess (List FullMovie)
    | RecommendationsFailure String



-- MODEL


type alias Model =
    { session : Session
    , id : MovieId
    , details : Status FullMovie
    , recommendations : RecommendationStatus
    , genres : Status (List Genre)
    }


init : Session -> MovieId -> ( Model, Cmd Msg )
init session id =
    let
        model =
            { session = session
            , id = id
            , details = Loading
            , recommendations = RecommendationsLoading Nothing
            , genres = Loading
            }
    in
    ( model
    , Cmd.batch
        [ Task.attempt GotGenres <| Genre.fetch session
        , Task.attempt GotDetails <| Movie.fetch session id
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
    | SelectedFavoriteMovie MovieId
    | RemovedFavoriteMovie MovieId


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
    let
        _ =
            case model.details of
                Loading ->
                    Debug.log "Loading" model.details

                Success _ ->
                    Debug.log "Details" model.details

                Failure _ ->
                    Debug.log "Failed" model.details
    in
    { title = "Movie " ++ MovieId.toString model.id
    , body =
        [ div
            [ css
                [ width (px 960)
                , margin2 zero auto
                , fontFamilies [ "Helvetica", "Arial", "serif" ]
                ]
            ]
            [ viewMovie model ]
        ]
    }


viewMovie : Model -> Html Msg
viewMovie model =
    case model.details of
        Loading ->
            text "Loading"

        Success movie ->
            Movie.view movie [] SelectedFavoriteMovie RemovedFavoriteMovie

        Failure err ->
            viewErrorMessage err


viewErrorMessage : String -> Html Msg
viewErrorMessage msg =
    div [ class "error" ] [ text msg ]
