module Session exposing
    ( Session
    , decode
    , encode
    , favoriteMovies
    , navKey
    , tokenQueryParam
    , updateWithStoredItems
    , year
    )

import Browser.Navigation as Nav
import Json.Decode as D exposing (Decoder, Value)
import Json.Decode.Pipeline as DP
import Json.Encode as E
import MovieId exposing (MovieId)



-- TYPES


{-| Session is the data shared between pages. There are token for API,
year (for footer) and data stored in local storage (favorite movies)
-}
type Session
    = Session Nav.Key Common (List MovieId)


{-| Common data is widely reused but doesn't need for
updates (loaded only on application setup via flags).
-}
type alias Common =
    { token : Maybe String
    , year : Maybe String
    }


emptyCommon : Common
emptyCommon =
    { token = Nothing
    , year = Nothing
    }



-- SERIALIZATION


favoriteMoviesDecoder : Decoder (List MovieId)
favoriteMoviesDecoder =
    D.succeed (Maybe.withDefault [])
        |> DP.optional "favoriteMovies" (D.nullable <| D.list MovieId.decoder) Nothing


commonDecoder : Decoder Common
commonDecoder =
    D.succeed Common
        |> DP.required "apiToken" (D.nullable D.string)
        |> DP.required "year" (D.nullable D.string)


decoder : Nav.Key -> Decoder Session
decoder key =
    favoriteMoviesDecoder
        |> D.map2 (Session key) commonDecoder


decode : Nav.Key -> Value -> Session
decode key value =
    case D.decodeValue (decoder key) value of
        Ok session ->
            session

        Err _ ->
            Session key
                emptyCommon
                []


encode : Session -> List MovieId -> E.Value
encode session fMovies =
    let
        (Session _ _ ids) =
            withFavoriteMovies fMovies session
    in
    E.object
        [ ( "favoriteMovies", E.list MovieId.encode <| ids ) ]



-- GETTERS


navKey : Session -> Nav.Key
navKey (Session key _ _) =
    key


favoriteMovies : Session -> List MovieId
favoriteMovies (Session _ _ fMovies) =
    fMovies


year : Session -> String
year (Session _ common _) =
    Maybe.withDefault "" common.year



-- QUERY PARAMS


tokenQueryParam : Session -> Maybe String
tokenQueryParam (Session _ common _) =
    Maybe.map ((++) "api_key=") common.token



-- TRANSFORMS


withFavoriteMovies : List MovieId -> Session -> Session
withFavoriteMovies fMovies (Session key token _) =
    Session key token fMovies


updateWithStoredItems : Session -> D.Value -> Session
updateWithStoredItems (Session key token _) storedFMovies =
    let
        fMovies =
            case D.decodeValue favoriteMoviesDecoder storedFMovies of
                Ok ids ->
                    ids

                Err _ ->
                    []
    in
    Session key token fMovies
