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
    , year
    )

import Browser.Navigation as Nav
import Json.Decode as D exposing (Decoder, Value)
import Json.Decode.Pipeline as DP
import Json.Encode as E
import MovieId exposing (MovieId)



-- TYPES


{-| Session is the data shared between pages. There are token for API,
year (for footer) and data stored in local storage (search query and favorite movies)
-}
type Session
    = Session Nav.Key Common Stored


{-| Common data is widely reused but doesn't need for
updates (loaded only on application setup via flags).
-}
type alias Common =
    { token : Maybe String
    , year : Maybe String
    }


{-| Stored data on the other hand needs updates.
We store search query and favorite movies in local storage so
JavaScript is the only source of truth for this data.
-}
type alias Stored =
    { query : Maybe String
    , favoriteMovies : Maybe (List MovieId)
    }


emptyCommon : Common
emptyCommon =
    { token = Nothing
    , year = Nothing
    }


emptyStored : Stored
emptyStored =
    { query = Nothing
    , favoriteMovies = Nothing
    }



-- SERIALIZATION


storedDecoder : Decoder Stored
storedDecoder =
    D.succeed Stored
        |> DP.required "query" (D.nullable D.string)
        |> DP.required "favoriteMovies" (D.nullable <| D.list MovieId.decoder)


commonDecoder : Decoder Common
commonDecoder =
    D.succeed Common
        |> DP.required "apiToken" (D.nullable D.string)
        |> DP.required "year" (D.nullable D.string)


decoder : Nav.Key -> Decoder Session
decoder key =
    storedDecoder
        |> D.map2 (Session key) commonDecoder


decode : Nav.Key -> Value -> Session
decode key value =
    case D.decodeValue (decoder key) value of
        Ok session ->
            session

        Err _ ->
            Session key
                emptyCommon
                emptyStored


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


year : Session -> String
year (Session _ common _) =
    Maybe.withDefault "" common.year



-- QUERY PARAMS


tokenQueryParam : Session -> Maybe String
tokenQueryParam (Session _ common _) =
    Maybe.map ((++) "api_key=") common.token


queryQueryParam : Session -> Maybe String
queryQueryParam (Session _ _ internals) =
    Maybe.map ((++) "query=") internals.query



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
            case D.decodeValue storedDecoder stored of
                Ok inter ->
                    inter

                Err _ ->
                    emptyStored
    in
    Session key token internals
