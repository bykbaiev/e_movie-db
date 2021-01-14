module Route exposing (Route(..), fromUrl, href)

import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attr
import MovieId exposing (MovieId)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)



-- ROUTING


type Route
    = Root
    | Movie MovieId


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Root Parser.top
        , Parser.map Movie (s "movie" </> MovieId.parser)
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


href : Route -> Attribute msg
href route =
    Attr.href (toString route)


toString : Route -> String
toString route =
    case route of
        Root ->
            "/"

        Movie id ->
            "/movie/" ++ MovieId.toString id
