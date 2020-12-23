module Session exposing
    ( Session
    , decode
    , encode
    , favoriteMovies
    , navKey
    , query
    , queryQueryParam
    , tokenQueryParam
    , updateWithStoredItems
    )

import Browser.Navigation as Nav
import Json.Decode as D exposing (Decoder, Value)
import Json.Decode.Pipeline as DP
import Json.Encode as E
import MovieId exposing (MovieId)



-- TYPES


type Session
    = Session Nav.Key Token Internals


type alias Token =
    Maybe String


type alias Internals =
    { query : Maybe String
    , favoriteMovies : Maybe (List MovieId)
    }


emptyInternals : Internals
emptyInternals =
    { query = Nothing
    , favoriteMovies = Nothing
    }



-- SERIALIZATION


internalsDecoder : Decoder Internals
internalsDecoder =
    D.succeed Internals
        |> DP.required "query" (D.nullable D.string)
        |> DP.required "favoriteMovies" (D.nullable <| D.list MovieId.decoder)


decoder : Nav.Key -> Decoder Session
decoder key =
    internalsDecoder
        |> D.andThen (addApiToken key)


addApiToken : Nav.Key -> Internals -> Decoder Session
addApiToken key internals =
    D.field "apiToken" (D.nullable D.string)
        |> D.map (\tok -> Session key tok internals)


decode : Nav.Key -> Value -> Session
decode key value =
    case D.decodeValue (decoder key) value of
        Ok session ->
            session

        Err _ ->
            Session key
                Nothing
                emptyInternals


encode : Session -> Maybe String -> Maybe (List MovieId) -> E.Value
encode session q fMovies =
    let
        (Session _ _ internals) =
            session
                |> withQuery q
                |> withFavoriteMovies fMovies
    in
    E.object
        [ ( "query", E.string <| Maybe.withDefault "" internals.query )
        , ( "favoriteMovies", E.list MovieId.encode <| Maybe.withDefault [] internals.favoriteMovies )
        ]



-- GETTERS


navKey : Session -> Nav.Key
navKey (Session key _ _) =
    key


query : Session -> String
query (Session _ _ internals) =
    Maybe.withDefault "" internals.query


favoriteMovies : Session -> List MovieId
favoriteMovies (Session _ _ internals) =
    Maybe.withDefault [] internals.favoriteMovies



-- QUERY PARAMS


tokenQueryParam : Session -> String
tokenQueryParam (Session _ token _) =
    case token of
        Just tok ->
            "api_key=" ++ tok

        Nothing ->
            ""


queryQueryParam : Session -> String
queryQueryParam (Session _ _ internals) =
    case internals.query of
        Just q ->
            "query=" ++ q

        Nothing ->
            ""



-- TRANSFORMS


withQuery : Maybe String -> Session -> Session
withQuery q (Session key token internals) =
    Session key token { internals | query = q }


withFavoriteMovies : Maybe (List MovieId) -> Session -> Session
withFavoriteMovies fMovies (Session key token internals) =
    Session key token { internals | favoriteMovies = fMovies }


updateWithStoredItems : Session -> D.Value -> Session
updateWithStoredItems (Session key token _) stored =
    let
        internals =
            case D.decodeValue internalsDecoder stored of
                Ok inter ->
                    inter

                Err _ ->
                    emptyInternals
    in
    Session key token internals
