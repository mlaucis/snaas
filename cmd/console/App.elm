module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, id, placeholder, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Navigation
import RemoteData exposing (RemoteData(..), WebData, sendRequest)

import Container
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
    }


type alias Model =
    { apps : WebData (List App)
    , description : String
    , name : String
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
    Model apps "" "" app


-- UPDATE


type Msg
    = FetchApp (WebData App)
    | FetchApps (WebData (List App))
    | Description String
    | List
    | Name String
    | New (Result Http.Error App)
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

        List ->
            ( model, Navigation.newUrl (Route.construct Route.Apps) )

        Name name ->
            ( { model | name = name }, Cmd.none )

        New (Ok app) ->
            -- TODO optimistic append with WebData.
            --( { model | apps = model.apps ++ [ app ], description = "", name = "" }, Cmd.none )
            ( { model | description = "", name = "" }, Cmd.none )

        New (Err _) ->
            ( model, Cmd.none )

        Select id ->
            ( model, Navigation.newUrl (Route.construct (Route.App id)) )

        Submit ->
            ( model, create model.name model.description )


-- VIEW


view : Context -> Html Msg
view { model, route } =
    let
        view =
            case route of
                Just (Route.Apps) ->
                    Container.view (section [ class "highlight" ])
                        [ viewList model.apps
                        , viewForm model
                        ]

                Just (Route.App _) ->
                    Container.view (section [ class "highlight" ])
                        [ viewApp model
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


viewApp : Model -> Html Msg
viewApp { selected } =
    case selected of
        NotAsked ->
            h3 [] [ text "Initialising" ]

        Loading ->
            h3 [] [ text "Loading" ]

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
                [ a [ onClick List ]
                    [ span [ class "icon nc-icon-glyph ui-2_layers" ] []
                    , span [] [ text "Apps" ]
                    ]
                ]
            , info
            ]


viewForm : Model -> Html Msg
viewForm model =
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


viewList : WebData (List App) -> Html Msg
viewList apps =
    case apps of
        NotAsked ->
            h3 [] [ text "Initialising" ]

        Loading ->
            h3 [] [ text "Loading" ]

        Failure err ->
            h3 [] [ text ("Error: " ++ toString err) ]

        Success apps ->
            if List.length apps == 0 then
                h3 [] [ text "Looks like you haven't created an App yet." ]
            else
                table []
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
        |> Http.send New


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
    Http.jsonBody (Encode.object [ ( "name", Encode.string name ), ( "description", Encode.string description ) ])


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
