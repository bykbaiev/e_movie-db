port module Ports exposing (onScroll, onSessionChange, scrollTop, storeSession)

import Json.Decode as D
import Json.Encode as E


port storeSession : E.Value -> Cmd msg


port scrollTop : () -> Cmd msg


port onSessionChange : (D.Value -> msg) -> Sub msg


port onScroll : (Int -> msg) -> Sub msg
