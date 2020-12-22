module Route exposing (Route(..), fromUrl)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, int, oneOf, s)



-- ROUTING


type Route
    = Root
    | Movie Int


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Root Parser.top
        , Parser.map Movie (s "movie" </> int)
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url
