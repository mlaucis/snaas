module App exposing (..)

import Color exposing (rgb)
import Html exposing (Html, a, button, div, form, h2, h3, input, nav, section, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, href, id, placeholder, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Navigation
import RemoteData exposing (RemoteData(..), WebData, sendRequest)
import Time exposing (Time)

import Container
import Loader
import Route


-- MODEL


type alias App =
    { backend_token : String
    , description : String
    , enabled : Bool
    , id : String
    , name : String
    , token : String
    }


type alias Context =
    { model : Model
    , route : Maybe Route.Route
    , startTime : Time
    , time : Time
    }


type alias Model =
    { apps : WebData (List App)
    , description : String
    , name : String
    , new : WebData App
    , selected : WebData App
    }


init : Maybe Route.Route -> ( Model, Cmd Msg )
init route =
    case route of
        Just (Route.App id) ->
            ( (initModel NotAsked Loading), getApp id )

        Just (Route.Apps) ->
            ( (initModel Loading NotAsked), getApps )

        _ ->
            ( (initModel NotAsked NotAsked), Cmd.none )


initApp : App
initApp =
    App "" "" False "" "" ""


initModel : WebData (List App) -> WebData App -> Model
initModel apps app =
    Model apps "" "" NotAsked app



-- UPDATE


type Msg
    = FetchApp (WebData App)
    | FetchApps (WebData (List App))
    | Description String
    | ListApps
    | Name String
    | New (WebData App)
    | Select String
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchApp response ->
            ( { model | selected = response }, Cmd.none )

        FetchApps response ->
            ( { model | apps = response, selected = NotAsked }, Cmd.none )

        Description description ->
            ( { model | description = description }, Cmd.none )

        ListApps ->
            ( model, Navigation.newUrl (Route.construct Route.Apps) )

        Name name ->
            ( { model | name = name }, Cmd.none )

        New response ->
            ( { model | apps = (combineApps model.apps response), new = NotAsked, description = "", name = "" }, Cmd.none )

        Select id ->
            ( model, Navigation.newUrl (Route.construct (Route.App id)) )

        Submit ->
            ( { model | new = Loading }, create model.name model.description )



-- VIEW


view : Context -> Html Msg
view { model, route, startTime, time } =
    let
        view =
            case route of
                Just (Route.Apps) ->
                    viewList model startTime time

                Just (Route.App _) ->
                    Container.view (section [ class "highlight" ])
                        [ viewApp model startTime time
                        ]

                _ ->
                    div []
                        [ span [] [ text "Route not found" ]
                        ]
    in
        div []
            [ viewContext model.selected
            , view
            ]


viewApp : Model -> Time -> Time -> Html Msg
viewApp { selected } startTime time =
    case selected of
        NotAsked ->
            h3 [] [ text "Initialising" ]

        Loading ->
            Loader.view 64 (rgb 63 91 96) (Loader.nextStep startTime time)

        Failure err ->
            h3 [] [ text ("Error: " ++ toString err) ]

        Success app ->
            h3 [] [ text ("App single view for " ++ app.name) ]


viewContext : WebData App -> Html Msg
viewContext app =
    let
        ( sectionClass, info ) =
            case app of
                Success app ->
                    ( "selected", viewSelected app )

                _ ->
                    ( "selected", span [] [] )
    in
        Container.view (section [ class sectionClass, id "context" ])
            [ h2 []
                [ a [ onClick ListApps ]
                    [ span [ class "icon nc-icon-glyph ui-2_layers" ] []
                    , span [] [ text "Apps" ]
                    ]
                ]
            , info
            ]


viewForm : Model -> Time -> Time -> Html Msg
viewForm model startTime time =
    let
        createForm =
            form [ onSubmit Submit ]
                [ input
                    [ onInput Name
                    , placeholder "Name"
                    , type_ "text"
                    , value model.name
                    ]
                    []
                , input
                    [ class "description"
                    , onInput Description
                    , placeholder "Description"
                    , type_ "text"
                    , value model.description
                    ]
                    []
                , button [ type_ "submit" ] [ text "Create" ]
                ]
    in
        case model.new of
            NotAsked ->
                createForm

            Loading ->
                Loader.view 48 (rgb 63 91 96) (Loader.nextStep startTime time)

            Failure err ->
                text ("Failed: " ++ toString err)

            Success _ ->
                createForm


viewItem : App -> Html Msg
viewItem app =
    let
        enabled =
            if app.enabled then
                span [ class "nc-icon-glyph ui-1_check-circle-07" ] []
            else
                span [ class "nc-icon-glyph ui-1_circle-remove" ] []
    in
        tr [ onClick (Select app.id) ]
            [ td [ class "status" ] [ enabled ]
            , td [] [ text app.name ]
            , td [] [ text app.description ]
            , td [] [ text app.token ]
            ]


viewList : Model -> Time -> Time -> Html Msg
viewList model startTime time =
    let
        content =
            case model.apps of
                NotAsked ->
                    [ h3 [] [ text "Initialising" ]
                    ]

                Loading ->
                    [ Loader.view 64 (rgb 63 91 96) (Loader.nextStep startTime time)
                    ]

                Failure err ->
                    [ h3 [] [ text ("Error: " ++ toString err) ]
                    ]

                Success apps ->
                    if List.length apps == 0 then
                        [ h3 [] [ text "Looks like you haven't created an App yet." ]
                        , viewForm model startTime time
                        ]
                    else
                        [ table []
                            [ thead []
                                [ tr []
                                    [ th [ class "status" ] [ text "status" ]
                                    , th [] [ text "name" ]
                                    , th [] [ text "description" ]
                                    , th [] [ text "token" ]
                                    ]
                                ]
                            , tbody [] (List.map viewItem apps)
                            ]
                        , viewForm model startTime time
                        ]
    in
        Container.view (section [ class "highlight" ])
            content


viewSelected : App -> Html Msg
viewSelected app =
    nav []
        [ a [ onClick (Select app.id), title app.name ]
            [ span [] [ text app.name ]
            , span [ class "icon nc-icon-outline arrows-2_skew-down" ] []
            ]
        ]



-- HTTP


create : String -> String -> Cmd Msg
create name description =
    Http.post "/api/apps" (encode name description) decode
        |> sendRequest
        |> Cmd.map New


decode : Decode.Decoder App
decode =
    Decode.map6 App
        (Decode.field "backend_token" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "enabled" Decode.bool)
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "token" Decode.string)


decodeList : Decode.Decoder (List App)
decodeList =
    Decode.at [ "apps" ] (Decode.list decode)


encode : String -> String -> Http.Body
encode name description =
    Encode.object
        [ ( "name", Encode.string name )
        , ( "description", Encode.string description )
        ]
        |> Http.jsonBody


getApp : String -> Cmd Msg
getApp id =
    Http.get ("/api/apps/" ++ id) decode
        |> sendRequest
        |> Cmd.map FetchApp


getApps : Cmd Msg
getApps =
    Http.get "/api/apps" decodeList
        |> sendRequest
        |> Cmd.map FetchApps



-- HELPER


combineApps : WebData (List App) -> WebData App -> WebData (List App)
combineApps apps app =
    case (RemoteData.toMaybe app) of
        Nothing ->
            apps

        Just app ->
            case (RemoteData.toMaybe apps) of
                Nothing ->
                    RemoteData.succeed [ app ]

                Just apps ->
                    RemoteData.succeed (apps ++ [ app ])
