module UI.Slider exposing (Slide, view)

import Css exposing (..)
import Html.Styled exposing (Html, div)
import Html.Styled.Keyed

type alias Slide msg = (String, Html msg)

view : List (Slide msg) -> Html msg
view slides =
    Html.Styled.Keyed.node
        "div"
        []
        (List.map viewSlide slides)

viewSlide : Slide msg -> (String, Html msg)
viewSlide (key, content) =
    (key, content)
