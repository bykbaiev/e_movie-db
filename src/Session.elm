module Session exposing
    ( Session
    , decode
    , favoriteMovies
    , navKey
    , query
    , withQuery
    , withToken
    )

import Browser.Navigation as Nav
import Json.Decode as D exposing (Decoder, Value)
import Json.Decode.Pipeline as DP
import MovieId exposing (MovieId)



-- TYPES


type Session
    = Session Nav.Key Internals


type alias Internals =
    { query : Maybe String
    , token : Maybe String
    , favoriteMovies : Maybe (List MovieId)
    }



-- SERIALIZATION


decoder : Nav.Key -> Decoder Session
decoder key =
    D.succeed Internals
        |> DP.required "query" (D.nullable D.string)
        |> DP.required "apiToken" (D.nullable D.string)
        |> DP.required "favoriteMovies" (D.nullable <| D.list MovieId.decoder)
        |> D.map (Session key)


decode : Nav.Key -> Value -> Session
decode key value =
    case D.decodeValue (decoder key) value of
        Ok session ->
            session

        Err _ ->
            Session key
                { query = Nothing
                , token = Nothing
                , favoriteMovies = Nothing
                }



-- GETTERS


navKey : Session -> Nav.Key
navKey (Session key _) =
    key


query : Session -> String
query (Session _ internals) =
    Maybe.withDefault "" internals.query


favoriteMovies : Session -> List MovieId
favoriteMovies (Session _ internals) =
    Maybe.withDefault [] internals.favoriteMovies



-- TRANSFORM


withToken : Session -> String -> String
withToken (Session _ internals) url =
    case internals.token of
        Just token ->
            url ++ "&api_key=" ++ token

        Nothing ->
            url


withQuery : Session -> String -> String
withQuery (Session _ internals) url =
    case internals.query of
        Just q ->
            url ++ "&query=" ++ q

        Nothing ->
            url
