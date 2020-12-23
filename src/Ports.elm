port module Ports exposing (onSessionChange, storeSession)

import Json.Decode as D
import Json.Encode as E


port storeSession : E.Value -> Cmd msg


port onSessionChange : (D.Value -> msg) -> Sub msg
