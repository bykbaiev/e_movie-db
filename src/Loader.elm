module Loader exposing (slowThreshold, view)

import Html.Styled exposing (Html, text)
import Process
import Task exposing (Task)


view : Html msg
view =
    text "Loading..."


slowThreshold : Task x ()
slowThreshold =
    Process.sleep 500
