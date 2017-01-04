module Console exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, id, placeholder, title, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Navigation
import App
import Container
import Route


main : Program Flags Model Msg
main =
    Navigation.programWithFlags LocationChange
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


-- MODEL


type alias Flags =
    { zone : String
    }


type alias Model =
    { appModel : App.Model
    , route : Route.Model
    , zone : String
    }


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init { zone } location =
    let
        ( routeModel, routeCmd ) =
            Route.init location

        ( appModel, appCmd ) =
            App.init routeModel.current
    in
        (Model appModel routeModel zone)
            ! [ Cmd.map RouteMsg routeCmd
              , Cmd.map AppMsg appCmd
              ]


-- UPDATE


type Msg
    = AppMsg App.Msg
    | LocationChange Navigation.Location
    | Navigate Route.Route
    | RouteMsg Route.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AppMsg appMsg ->
            let
                ( appModel, appCmd ) =
                    App.update appMsg model.appModel
            in
                ( { model | appModel = appModel }, Cmd.map AppMsg appCmd )

        LocationChange location ->
            init (Flags model.zone) location

        Navigate route ->
            ( model, Cmd.map RouteMsg (Route.navigate route) )

        RouteMsg routeMsg ->
            let
                ( routeModel, routeCmd ) =
                    Route.update routeMsg model.route
            in
                ( { model | route = routeModel }, Cmd.map RouteMsg routeCmd )


-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW


view : Model -> Html Msg
view model =
    let
        page =
            case model.route.current of
                Nothing ->
                    [ viewNotFound ]

                Just (Route.App _) ->
                    [ Html.map AppMsg (App.view (App.Context model.appModel model.route.current))
                    ]

                Just (Route.Apps) ->
                    [ Html.map AppMsg (App.view (App.Context model.appModel model.route.current))
                    ]

                Just (Route.Dashboard) ->
                    [ viewDashboard model.zone ]

                Just (Route.Members) ->
                    [ viewNotFound ]
    in
        div [ class "content" ]
            ([ viewHeader model ] ++ page ++ [ viewFooter model ])


viewDashboard : String -> Html Msg
viewDashboard zone =
    Container.view (section [ id "dashboard" ])
        [ h2 []
            [ text "Hej, welcome to your installation in"
            , span [ class "zone" ] [ text zone ]
            , text "start of by looking into"
            , a [ onClick (Navigate Route.Apps), title "Apps" ]
                [ span [ class "icon nc-icon-glyph ui-2_layers" ] []
                , text "Apps"
                ]
            , text "or"
            , a [ onClick (Navigate Route.Members), title "Members" ]
                [ span [ class "icon nc-icon-glyph users_multiple-11" ] []
                , text "Members"
                ]
            ]
        ]


viewDebug : Model -> Html Msg
viewDebug model =
    div [ class "debug" ]
        [ text (toString model)
        ]


viewFooter : Model -> Html Msg
viewFooter model =
    Container.view (footer []) [ viewDebug model ]


viewHeader : Model -> Html Msg
viewHeader model =
    Container.view (header [])
        [ h1 []
            [ a [ onClick (Navigate Route.Dashboard), title "Home" ]
                [ strong [] [ text "SocialPath" ]
                , span [] [ text "Console" ]
                ]
            ]
        , nav [] [ span [] [ text model.zone ] ]
        ]


viewNotFound : Html Msg
viewNotFound =
    Container.view (section [ class "highlight" ])
        [ h3 [] [ text "Looks like we couldn't find the page you were looking for." ]
        ]
