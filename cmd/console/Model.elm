module Model exposing (Flags, Model, init)

import Navigation
import RemoteData exposing (RemoteData(..), WebData)
import Time exposing (Time)

import Action exposing (..)
import Api exposing (getApp, getApps)
import App exposing (App)
import Route exposing (Route, parse)

type alias Flags =
    { zone : String
    }

type alias Model =
    { app : WebData App
    , apps : WebData (List App)
    , appDescription : String
    , appName : String
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
                ( (model Loading NotAsked), getApp id )

            Just (Route.Apps) ->
                ( (model NotAsked Loading), getApps )

            _ ->
                ( (model NotAsked NotAsked), Cmd.none)

initModel : String -> Maybe Route -> WebData App -> WebData (List App) -> Model
initModel zone route app apps =
    Model app apps "" "" NotAsked route 0 0 zone
