module SearchOptions exposing (..)

import Html exposing (Html, div, input, label, option, select, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (on, targetValue)
import Json.Decode
import Regex exposing (Regex)


languages : List String
languages =
    [ "en", "de" ]


regions : List String
regions =
    [ "US", "GB", "DE" ]


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


yearRegex : Regex
yearRegex =
    Maybe.withDefault Regex.never <| Regex.fromString "^[12]\\d{3}$"


updateOptions : Msg -> Options -> Options
updateOptions msg options =
    case msg of
        SetLanguage language ->
            { options | language = language }

        SetIncludeAdult includeAdult ->
            { options | includeAdult = includeAdult }

        SetRegion region ->
            { options | region = region }

        SetYear year ->
            { options | year = year }

        SetPrimaryReleaseYear primaryReleaseYear ->
            { options | primaryReleaseYear = primaryReleaseYear }


onBlurWithTargetValue : (String -> msg) -> Html.Attribute msg
onBlurWithTargetValue toMsg =
    on "blur" (Json.Decode.map toMsg targetValue)


onChange : (String -> msg) -> Html.Attribute msg
onChange toMsg =
    on "change" (Json.Decode.map toMsg targetValue)


stringToLanguage : String -> Maybe String
stringToLanguage language =
    if List.any (\lang -> lang == language) languages then
        Just language

    else
        Nothing


stringToRegion : String -> Maybe String
stringToRegion region =
    if List.any (\reg -> reg == region) regions then
        Just region

    else
        Nothing


type alias SelectOption =
    { value : String
    , text : String
    }


viewOption : SelectOption -> Html msg
viewOption selectOption =
    option [ value selectOption.value ] [ text selectOption.text ]


viewSelect : List { value : String, text : String } -> String -> String -> (String -> msg) -> Html msg
viewSelect options labelText selectedValue selectOpt =
    div [ class "search-option" ]
        [ label [ class "top-label" ] [ text labelText ]
        , select [ onChange selectOpt, value selectedValue ]
            (List.map viewOption options)
        ]


view : Options -> Html Msg
view options =
    div [ class "search-options" ]
        [ viewSelect
            [ { value = "", text = "None" }
            , { value = "en", text = "English" }
            , { value = "de", text = "German" }
            ]
            "Language"
            (Maybe.withDefault "" options.language)
            (SetLanguage << stringToLanguage)
        , viewSelect
            [ { value = "", text = "None" }
            , { value = "GB", text = "United Kingdom" }
            , { value = "US", text = "United States" }
            , { value = "DE", text = "Germany" }
            ]
            "Region"
            (Maybe.withDefault "" options.region)
            (SetRegion << stringToRegion)

        -- , div [ class "search-option" ]
        --     [ label [ class "top-label" ] [ text "Year" ]
        --     , input
        --         [ type_ "text"
        --         , placeholder "Enter a year"
        --         , defaultValue options.year
        --         , onInput SetYear
        --         ]
        --         []
        --     ]
        -- , div [ class "search-option" ]
        --     [ label [ class "top-label" ] [ text "Primary year" ]
        --     , input
        --         [ type_ "text"
        --         , onBlurWithTargetValue SetPrimaryReleaseYear
        --         , value (Maybe.withDefault "" <| Maybe.map String.fromInt options.primaryReleaseYear)
        --         ]
        --         []
        --     -- , viewMinStarsError opts.minStarsError
        --     ]
        ]
