module SearchOptions exposing (..)

import Html exposing (Html, div, text)


type alias Options =
    { language : Maybe String
    , includeAdult : Bool
    , region : Maybe String
    , year : Maybe Int
    , primaryReleaseYear : Maybe Int
    }


type Msg
    = SetLanguage (Maybe String)
    | SetIncludeAdult Bool
    | SetRegion (Maybe String)
    | SetYear (Maybe Int)
    | SetPrimaryReleaseYear (Maybe Int)


initialModel : Options
initialModel =
    { language = Nothing
    , includeAdult = False
    , region = Nothing
    , year = Nothing
    , primaryReleaseYear = Nothing
    }


update : Msg -> Options -> ( Options, Cmd Msg )
update msg options =
    case msg of
        SetLanguage language ->
            ( { options | language = language }, Cmd.none )

        SetIncludeAdult includeAdult ->
            ( { options | includeAdult = includeAdult }, Cmd.none )

        SetRegion region ->
            ( { options | region = region }, Cmd.none )

        SetYear year ->
            ( { options | year = year }, Cmd.none )

        SetPrimaryReleaseYear primaryReleaseYear ->
            ( { options | primaryReleaseYear = primaryReleaseYear }, Cmd.none )


view : Options -> Html Msg
view options =
    div [] [ text "Search options" ]
