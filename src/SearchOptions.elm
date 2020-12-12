module SearchOptions exposing (..)

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, selected, type_, value)
import Html.Styled.Events exposing (on, onClick, targetValue)
import Json.Decode
import Regex exposing (Regex)


languages : List String
languages =
    [ "en", "de" ]


regions : List String
regions =
    [ "US", "GB", "DE" ]


emptyOptionValue : String
emptyOptionValue =
    "none"


type alias Options =
    { opened : Bool
    , language : Maybe String
    , includeAdult : Bool
    , region : Maybe String
    , year : Maybe Int
    , primaryReleaseYear : Maybe Int
    }


type Msg
    = SetOpened Bool
    | SetLanguage (Maybe String)
    | SetIncludeAdult Bool
    | SetRegion (Maybe String)
    | SetYear (Maybe Int)
    | SetPrimaryReleaseYear (Maybe Int)


initialModel : Options
initialModel =
    { opened = False
    , language = Nothing
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
        SetOpened opened ->
            { options | opened = opened }

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


onBlurWithTargetValue : (String -> msg) -> Attribute msg
onBlurWithTargetValue toMsg =
    on "blur" (Json.Decode.map toMsg targetValue)


onChange : (String -> msg) -> Attribute msg
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


viewOption : String -> SelectOption -> Html msg
viewOption selectedValue selectOption =
    option
        [ selected (selectedValue == selectOption.value)
        , value selectOption.value
        ]
        [ text selectOption.text ]


viewSelect : List { value : String, text : String } -> String -> String -> (String -> msg) -> Html msg
viewSelect options labelText selectedValue selectOpt =
    div [ class "search-option" ]
        [ label
            [ css
                [ display block
                , marginBottom (px 8)
                , fontSize (px 14)
                , fontWeight (int 500)
                ]
            ]
            [ text labelText ]
        , select [ onChange selectOpt, value selectedValue ]
            (List.map (viewOption selectedValue) options)
        ]


view : Options -> Html Msg
view options =
    let
        optionsContainer =
            if options.opened then
                viewOptions options

            else
                text ""
    in
    div []
        [ div
            [ css
                [ displayFlex
                , justifyContent spaceBetween
                , alignItems center
                ]
            ]
            [ p [] [ text "Additional options" ]
            , button [ onClick <| SetOpened (not options.opened) ] [ text "Toggle additional options" ]
            ]
        , optionsContainer
        ]


optionsItemStyle : List Style
optionsItemStyle =
    [ marginRight (px 8) ]


viewOptions : Options -> Html Msg
viewOptions options =
    div
        [ css
            [ displayFlex
            , alignItems center
            , padding2 (px 8) zero
            ]
        ]
        [ div [ css optionsItemStyle ]
            [ viewSelect
                [ { value = emptyOptionValue, text = "None" }
                , { value = "en", text = "English" }
                , { value = "de", text = "German" }
                ]
                "Language"
                (Maybe.withDefault emptyOptionValue options.language)
                (SetLanguage << stringToLanguage)
            ]
        , div [ css optionsItemStyle ]
            [ viewSelect
                [ { value = emptyOptionValue, text = "None" }
                , { value = "GB", text = "United Kingdom" }
                , { value = "US", text = "United States" }
                , { value = "DE", text = "Germany" }
                ]
                "Region"
                (Maybe.withDefault emptyOptionValue options.region)
                (SetRegion << stringToRegion)
            ]

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
