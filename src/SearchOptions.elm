module SearchOptions exposing
    ( Msg
    , Options
    , adultQueryParam
    , initialModel
    , languageQueryParam
    , regionQueryParam
    , updateOptions
    , view
    )

import Css exposing (..)
import Html.Attributes exposing (style)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (class, css, selected, type_, value)
import Html.Styled.Events exposing (on, onClick, targetValue)
import Json.Decode



-- TYPES


type Options
    = Options
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
    | ToggleIncludeAdult
    | SetRegion (Maybe String)
    | SetYear (Maybe Int)
    | SetPrimaryReleaseYear (Maybe Int)


type alias SelectOption =
    { value : String
    , text : String
    }



-- MODEL


initialModel : Options
initialModel =
    Options
        { opened = False
        , language = Nothing
        , includeAdult = False
        , region = Nothing
        , year = Nothing
        , primaryReleaseYear = Nothing
        }



-- UPDATE
{-
   Update function is slightly custom. It's not the
   standard TEA update: we return not the model and the command
   but the model and the flag if movies should be reloaded.

   The reason is that there is opened state which indicates if it's the
   advanced search settings mode or not. And the search request on opened state
   change should be prevented.
-}


updateOptions : Msg -> Options -> ( Options, Bool )
updateOptions msg (Options options) =
    case msg of
        SetOpened opened ->
            ( Options { options | opened = opened }, False )

        SetLanguage language ->
            ( Options { options | language = language }, True )

        ToggleIncludeAdult ->
            ( Options { options | includeAdult = not options.includeAdult }, True )

        SetRegion region ->
            ( Options { options | region = region }, True )

        SetYear year ->
            ( Options { options | year = year }, True )

        SetPrimaryReleaseYear primaryReleaseYear ->
            ( Options { options | primaryReleaseYear = primaryReleaseYear }, True )



-- VIEW


view : Options -> Html Msg
view (Options options) =
    let
        optionsContainer =
            if options.opened then
                viewOptions (Options options)

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


viewOption : String -> SelectOption -> Html msg
viewOption selectedValue selectOption =
    option
        [ selected (selectedValue == selectOption.value)
        , value selectOption.value
        ]
        [ text selectOption.text ]


viewSelect : List { value : String, text : String } -> { labelText : String, selectedValue : String } -> (String -> msg) -> Html msg
viewSelect options { labelText, selectedValue } selectOpt =
    div [ class "search-option" ]
        [ label [ css labelStyles ] [ text labelText ]
        , select [ onChange selectOpt, value selectedValue ]
            (List.map (viewOption selectedValue) options)
        ]


viewOptions : Options -> Html Msg
viewOptions (Options options) =
    div
        [ css
            [ displayFlex
            , alignItems center
            , padding2 (px 8) zero
            ]
        ]
        [ div [ css optionsItemStyles ]
            [ viewSelect
                [ { value = emptyOptionValue, text = "None" }
                , { value = "en", text = "English" }
                , { value = "de", text = "German" }
                ]
                { labelText = "Language"
                , selectedValue = Maybe.withDefault emptyOptionValue options.language
                }
                (SetLanguage << stringToLanguage)
            ]
        , div [ css optionsItemStyles ]
            [ viewSelect
                [ { value = emptyOptionValue, text = "None" }
                , { value = "GB", text = "United Kingdom" }
                , { value = "US", text = "United States" }
                , { value = "DE", text = "Germany" }
                ]
                { labelText = "Region"
                , selectedValue = Maybe.withDefault emptyOptionValue options.region
                }
                (SetRegion << stringToRegion)
            ]
        , div [ css optionsItemStyles ]
            [ label [ css labelStyles ] [ text "Include adults?" ]
            , input
                [ type_ "checkbox"
                , Html.Styled.Attributes.checked options.includeAdult
                , onClick ToggleIncludeAdult
                ]
                []
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



-- STYLES


labelStyles : List Style
labelStyles =
    [ display block
    , marginBottom (px 8)
    , fontSize (px 14)
    , fontWeight (int 500)
    ]


optionsItemStyles : List Style
optionsItemStyles =
    [ marginRight (px 8) ]



-- CONSTANTS
{-
   Constants related to search options form fields
-}


languages : List String
languages =
    [ "en", "de" ]


regions : List String
regions =
    [ "US", "GB", "DE" ]


emptyOptionValue : String
emptyOptionValue =
    "none"



-- EVENTS


onBlurWithTargetValue : (String -> msg) -> Attribute msg
onBlurWithTargetValue toMsg =
    on "blur" (Json.Decode.map toMsg targetValue)


onChange : (String -> msg) -> Attribute msg
onChange toMsg =
    on "change" (Json.Decode.map toMsg targetValue)



-- MAPPERS


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



-- QUERY PARAMS


languageQueryParam : Options -> Maybe String
languageQueryParam (Options options) =
    Maybe.map ((++) "language=") options.language


regionQueryParam : Options -> Maybe String
regionQueryParam (Options options) =
    Maybe.map ((++) "region=") options.region


adultQueryParam : Options -> Maybe String
adultQueryParam (Options options) =
    Just <|
        "include_adult="
            ++ (if options.includeAdult then
                    "true"

                else
                    "false"
               )
