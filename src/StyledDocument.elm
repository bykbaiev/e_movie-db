module StyledDocument exposing (StyledDocument, toDocument)

import Browser exposing (Document)
import Html.Styled



-- TYPES


type alias StyledDocument msg =
    { title : String
    , body : List (Html.Styled.Html msg)
    }



-- TRANSFORMS


toDocument : StyledDocument msg -> Document msg
toDocument { title, body } =
    { title = title
    , body = List.map Html.Styled.toUnstyled body
    }
