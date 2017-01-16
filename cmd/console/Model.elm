module Model exposing (Flags, Model, init)

import Navigation
import RemoteData exposing (RemoteData(..), WebData)
import Time exposing (Time)

import Action exposing (..)
import App.Api exposing (getApp, getApps)
import App.Model exposing (App, initAppForm)
import Formo exposing (Form)
import Route exposing (Route, parse)

type alias Flags =
    { zone : String
    }

type alias Model =
    { app : WebData App
    , apps : WebData (List App)
    , appForm : Form
    , focus : String
    , newApp : WebData App
    , route : Maybe Route
    , startTime : Time
    , time : Time
    , zone : String
    }

init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init { zone } location =
    let
        route = parse location

        model = initModel zone route
    in
        case route of
            Just (Route.App id) ->
                ( (model Loading NotAsked), Cmd.map FetchApp (getApp id) )

            Just (Route.Apps) ->
                ( (model NotAsked Loading), Cmd.map FetchApps getApps )

            _ ->
                ( (model NotAsked NotAsked), Cmd.none)

initModel : String -> Maybe Route -> WebData App -> WebData (List App) -> Model
initModel zone route app apps =
    Model app apps initAppForm "" NotAsked route 0 0 zone
