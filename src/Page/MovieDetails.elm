module Page.MovieDetails exposing (..)

import Genre exposing (Genre)
import Html.Styled exposing (Html, div, text)
import Http
import Movie exposing (FullMovie)
import MovieId exposing (MovieId)
import Ports exposing (onSessionChange)
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



-- VIEW


view : Model -> StyledDocument Msg
view model =
    { title = "Movie " ++ MovieId.toString model.id
    , body =
        [ div [] [ text "Movie details page" ] ]
    }
