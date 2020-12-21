module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html.Attributes exposing (title)
import Html.Styled exposing (..)
import Json.Decode as D exposing (Value)
import Page.Home as Home
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)



-- TYPES


type Model
    = Home Home.Model
    | NotFound Session



-- MODEL


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        session =
            Session.decode navKey flags
    in
    changeRouteTo (Route.fromUrl url) (NotFound session)


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none



-- Sub.map Home Home.subscriptions model
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

        Just _ ->
            let
                ( newModel, cmd ) =
                    Home.init session
            in
            ( Home newModel, Cmd.map GotHomeMsg cmd )


getSession : Model -> Session
getSession model =
    case model of
        Home home ->
            home.session

        NotFound session ->
            session



-- VIEW


view : Model -> { title : String, body : List (Html msg) }
view model =
    { title = "Movie data base"
    , body =
        [ div [] [ text "New application?!" ] ]
    }



-- MAIN


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = documentToUnstyled << view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }


documentToUnstyled : { title : String, body : List (Html msg) } -> Document msg
documentToUnstyled { title, body } =
    { title = title
    , body = List.map toUnstyled body
    }
