module RequestHelpers exposing (handleJsonResponse, queryParam)

import Http
import Json.Decode exposing (Decoder)


handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result


queryParam : { name : String, value : String, isFirst : Bool } -> String
queryParam { name, value, isFirst } =
    let
        symb =
            if isFirst then
                "?"

            else
                "&"
    in
    if value /= "" then
        symb ++ name ++ "=" ++ value

    else
        ""
