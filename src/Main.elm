module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Json.Decode exposing (Value)
import Page.Home as Home
import Route exposing (Route)
import Session exposing (Session)
import StyledDocument exposing (StyledDocument)
import Url exposing (Url)



-- TYPES


type Model
    = Home Home.Model
    | NotFound Session



-- MODEL


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        _ =
            Debug.log "flags" flags

        session =
            Session.decode navKey flags
    in
    changeRouteTo (Route.fromUrl url) (NotFound session)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Home home ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        NotFound _ ->
            Sub.none



-- UPDATE


type Msg
    = GotHomeMsg Home.Msg
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotHomeMsg homeMsg, Home home ) ->
            let
                ( newModel, cmd ) =
                    Home.update homeMsg home
            in
            ( Home newModel, Cmd.map GotHomeMsg cmd )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (Session.navKey <| getSession model) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( _, _ ) ->
            ( model, Cmd.none )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            getSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Root ->
            Home.init session
                |> updateModelWith Home GotHomeMsg

        Just _ ->
            ( NotFound session, Cmd.none )


updateModelWith : (model -> Model) -> (msg -> Msg) -> ( model, Cmd msg ) -> ( Model, Cmd Msg )
updateModelWith toModel toMsg ( model, msg ) =
    ( toModel model, Cmd.map toMsg msg )


getSession : Model -> Session
getSession model =
    case model of
        Home home ->
            home.session

        NotFound session ->
            session



-- VIEW


view : Model -> StyledDocument Msg
view model =
    let
        content =
            case model of
                Home home ->
                    let
                        { title, body } =
                            Home.view home
                    in
                    { title = title, body = List.map (Html.map GotHomeMsg) body }

                NotFound session ->
                    { title = "Movie data base"
                    , body =
                        [ div [] [ text "New application?!" ] ]
                    }
    in
    { title = content.title
    , body =
        [ div
            [ css
                [ displayFlex
                , flexDirection column
                , justifyContent spaceBetween
                , height (vh 100)
                ]
            ]
            [ viewHeader
            , div
                [ css
                    [ flex (int 1)
                    , padding (px 8)
                    ]
                ]
                content.body
            , viewFooter <| getSession model
            ]
        ]
    }


viewHeader : Html msg
viewHeader =
    header
        [ css
            [ padding (px 16)
            , backgroundColor (rgb 96 181 204)
            , textAlign center
            , color (hex "#fff")
            ]
        ]
        [ h1 [ css [ fontSize (px 36) ] ] [ text "Movies App" ]
        , h2
            [ css [ fontSize (px 24) ] ]
            [ text "Keep all movies you link in one place" ]
        ]


viewFooter : Session -> Html msg
viewFooter session =
    footer
        [ css
            [ padding (px 16)
            , backgroundColor (rgb 96 181 204)
            , textAlign center
            , color (hex "#fff")
            ]
        ]
        [ h2
            [ css [ fontSize (px 24) ] ]
            [ session
                |> Session.year
                |> (\y -> y ++ " None rights reserved =)")
                |> text
            ]
        ]



-- MAIN


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = StyledDocument.toDocument << view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
