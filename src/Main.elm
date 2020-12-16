module Main exposing (..)

import Browser
import Html.Styled exposing (..)
import MainDB


main : Program MainDB.Flags MainDB.Model MainDB.Msg
main =
    Browser.element
        { init = MainDB.init
        , view = toUnstyled << MainDB.view
        , update = MainDB.update
        , subscriptions = MainDB.subscriptions
        }
