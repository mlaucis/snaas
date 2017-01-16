module View exposing (view)

import Char
import Color exposing (rgb)
import Html exposing (..)
import Html.Attributes exposing (class, href, id, placeholder, title, type_, value)
import Html.Events exposing (onClick, onFocus, onInput, onSubmit)
import RemoteData exposing (RemoteData(Failure, Loading, NotAsked, Success), WebData)
import Time exposing (Time)

import Action exposing (..)
import App.Model exposing (App)
import App.View exposing (viewAppItem, viewAppsContext, viewAppsTable)
import Container
import Formo exposing (Form, elementError, elementValue)
import Loader
import Model exposing (Model)
import Route

view : Model -> Html Msg
view model =
    let
        page =
            case model.route of
                Nothing ->
                    pageNotFound

                Just (Route.App _) ->
                    pageApp model

                Just (Route.Apps) ->
                    pageApps model

                Just (Route.Dashboard) ->
                    pageDashboard model.zone

                Just (Route.Members) ->
                    pageNotFound
    in
        div [ class "content" ]
            ([ viewHeader model.zone ] ++ [ page ] ++ [ viewFooter model ])

pageApp : Model -> Html Msg
pageApp {app, startTime, time} =
    let
        view =
            case app of
                NotAsked ->
                    h3 [] [ text "Initialising" ]

                Loading ->
                    Loader.view 64 (rgb 63 91 96) (Loader.nextStep startTime time)

                Failure err ->
                    h3 [] [ text ("Error: " ++ toString err) ]

                Success app ->
                    h3 [] [ text ("App single view for " ++ app.name) ]
    in
        div []
            [ viewAppsContext (Navigate Route.Apps) app
            , Container.view (section [ class "highlight" ])
                [ view
                ]
            ]

pageApps : Model -> Html Msg
pageApps {app, apps, appForm, newApp, startTime, time} =
    let
        viewItem =
            (\app -> viewAppItem (SelectApp app.id) app)

        content =
            case apps of
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
                        , formApp newApp appForm startTime time
                        ]
                    else
                        [ viewAppsTable viewItem apps
                        , formApp newApp appForm startTime time
                        ]
    in
        div []
            [ viewAppsContext (Navigate Route.Apps) app
            , Container.view (section [ class "highlight" ]) content
            ]

pageDashboard : String -> Html Msg
pageDashboard zone =
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

pageNotFound : Html Msg
pageNotFound =
    Container.view (section [ class "highlight" ])
        [ h3 [] [ text "Looks like we couldn't find the page you were looking for." ]
        ]

viewDebug : Model -> Html Msg
viewDebug model =
    div [ class "debug" ]
        [ text (toString model)
        ]


viewHeader : String -> Html Msg
viewHeader zone =
    Container.view (header [])
        [ h1 []
            [ a [ onClick (Navigate Route.Dashboard), title "Home" ]
                [ strong [] [ text "SocialPath" ]
                , span [] [ text "Console" ]
                ]
            ]
        , nav [] [ span [] [ text zone ] ]
        ]


viewFooter : Model -> Html Msg
viewFooter model =
    Container.view (footer [])
        [ viewDebug model
        ]


-- FORM


formApp : WebData App -> Form -> Time -> Time -> Html Msg
formApp new appForm startTime time =
    let
        createForm =
            form [ onSubmit AppFormSubmit ]
                [ formElementText (AppFormFocus "name") (AppFormUpdate "name") appForm "name"
                , formElementText (AppFormFocus "description") (AppFormUpdate "description") appForm "description"
                , button [ type_ "submit" ] [ text "Create" ]
                ]
    in
        case new of
            NotAsked ->
                createForm

            Loading ->
                Loader.view 48 (rgb 63 91 96) (Loader.nextStep startTime time)

            Failure err ->
                text ("Failed: " ++ toString err)

            Success _ ->
                createForm

formElementText : Msg -> (String -> Msg) -> Form -> String -> Html Msg
formElementText focusMsg inputMsg form field =
    div [ class "form-group" ]
        [ input
            [ class field
            , onFocus focusMsg
            , onInput inputMsg
            , placeholder (capitalise field)
            , type_ "text"
            , value (elementValue form field)
            ]
            []
        , span []
            [ text (elementError form field) ]
        ]


-- HELPER

capitalise : String -> String
capitalise s =
    case String.uncons s of
        Nothing ->
            ""

        Just (head, tail) ->
            String.cons (Char.toUpper head) tail
