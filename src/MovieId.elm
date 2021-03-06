module MovieId exposing
    ( MovieId
    , decoder
    , encode
    , fromInt
    , parser
    , toInt
    , toString
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Url.Parser exposing (Parser)


type MovieId
    = MovieId Int



-- CREATE


decoder : Decoder MovieId
decoder =
    D.map MovieId D.int


encode : MovieId -> E.Value
encode (MovieId id) =
    E.int id



-- PARSING


parser : Parser (MovieId -> a) a
parser =
    Url.Parser.custom "USERNAME" <| Maybe.map MovieId << String.toInt



-- MAPPING


toString : MovieId -> String
toString (MovieId id) =
    String.fromInt id


toInt : MovieId -> Int
toInt (MovieId id) =
    id


{-| It's better to avoid manual creating of movie identifier
but so far the favorite movies have been kept only in local storage. So it seems
like with our non-ideal architecture there is need to mapping between JS data
and Elm model.

To refactor this please keep all the data on server.

-}
fromInt : Int -> MovieId
fromInt id =
    MovieId id
