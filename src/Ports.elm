port module Ports exposing (..)


port storeQuery : String -> Cmd msg


port storeFavoriteMovies : List Int -> Cmd msg


port onFavoriteMoviesChange : (List Int -> msg) -> Sub msg
