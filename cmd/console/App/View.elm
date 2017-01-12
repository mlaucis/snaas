module App.View exposing (viewAppItem, viewAppsContext, viewAppsTable)

import Html exposing (Html, a, h2, nav, section, span, table, tbody, td, th, thead, tr, text)
import Html.Attributes exposing (class, id, title)
import Html.Events exposing (onClick)
import RemoteData exposing (RemoteData(Success), WebData)

import App.Model exposing (App)
import Container

viewAppsContext : msg -> WebData App -> Html msg
viewAppsContext msg app =
    let
        ( sectionClass, info ) =
            case app of
                Success app ->
                    ( "selected", viewAppSelected app )

                _ ->
                    ( "selected", span [] [] )
    in
        Container.view (section [ class sectionClass, id "context" ])
            [ h2 []
                [ a [ onClick msg ]
                    [ span [ class "icon nc-icon-glyph ui-2_layers" ] []
                    , span [] [ text "Apps" ]
                    ]
                ]
            , info
            ]

viewAppItem : msg -> App -> Html msg
viewAppItem msg app =
    let
        enabled =
            if app.enabled then
                span [ class "nc-icon-glyph ui-1_check-circle-07" ] []
            else
                span [ class "nc-icon-glyph ui-1_circle-remove" ] []
    in
        tr [ onClick msg ]
            [ td [ class "status" ] [ enabled ]
            , td [] [ text app.name ]
            , td [] [ text app.description ]
            , td [] [ text app.token ]
            ]

viewAppSelected : App -> Html msg
viewAppSelected app =
    nav []
        [ a []
            [ span [] [ text app.name ]
            , span [ class "icon nc-icon-outline arrows-2_skew-down" ] []
            ]
        ]

viewAppsTable : (App -> Html msg) -> List App -> Html msg
viewAppsTable item apps =
    table []
        [ thead []
            [ tr []
                [ th [ class "status" ] [ text "status" ]
                , th [] [ text "name" ]
                , th [] [ text "description" ]
                , th [] [ text "token" ]
                ]
            ]
        , tbody [] (List.map item apps)
        ]
