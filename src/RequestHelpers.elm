module RequestHelpers exposing
    ( fetch
    , handleJsonResponse
    , queryParam
    , toString
    )

import Http
import Json.Decode exposing (Decoder)
import Task exposing (Task)


fetch : String -> Decoder a -> Task Http.Error a
fetch url decoder =
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse <| decoder
        , timeout = Nothing
        }


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


toString : Http.Error -> String
toString error =
    case error of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Timeout is reached"

        Http.NetworkError ->
            "There are some network errors. Please, check your connection"

        Http.BadStatus status ->
            "The requests failed with status code " ++ String.fromInt status

        Http.BadBody message ->
            "The request failed with some bad body: " ++ message
