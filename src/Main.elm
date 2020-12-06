module Main exposing (..)

import Browser
import MainDB


main : Program () MainDB.Model MainDB.Msg
main =
    Browser.element { init = MainDB.init, view = MainDB.view, update = MainDB.update, subscriptions = \_ -> Sub.none }
