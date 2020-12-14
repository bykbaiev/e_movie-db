module Genre exposing (Genre, genreDecoder)

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)



-- TYPES


type alias Genre =
    { id : Int
    , name : String
    }



-- SERIALIZATION


genreDecoder : Decoder Genre
genreDecoder =
    Json.Decode.succeed Genre
        |> required "id" Json.Decode.int
        |> required "name" Json.Decode.string
