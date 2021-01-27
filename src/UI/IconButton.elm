module UI.IconButton exposing (iconButton)

import Css exposing (..)
import Css.Transitions as Transitions exposing (transition)
import Html.Styled exposing (Html, button)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Svg exposing (Svg)
import Svg.Styled exposing (svg)
import Svg.Styled.Attributes as SvgAttr


iconButton : Maybe (List Style) -> msg -> Int -> Svg msg -> Html msg
iconButton maybeStyle msg size icon =
    let
        style =
            [ border zero
            , width <| px 40
            , height <| px 40
            , borderRadius <| pct 50
            , backgroundColor <| hex "eee"
            , cursor pointer
            , textAlign center
            , hover [ backgroundColor <| hex "ddd", transform (scale 1.2) ]
            , transition
                [ Transitions.backgroundColor 1000
                , Transitions.transform2 500 0
                ]
            ]
                ++ Maybe.withDefault [] maybeStyle

        svgSize =
            String.fromInt size
    in
    button [ css style, onClick msg ]
        [ svg
            [ SvgAttr.width svgSize, SvgAttr.height svgSize ]
            [ Svg.Styled.fromUnstyled icon ]
        ]
