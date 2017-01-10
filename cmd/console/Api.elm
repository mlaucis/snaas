module Api exposing (createApp, getApp, getApps)

import Http
import RemoteData exposing (sendRequest)

import Action exposing (Msg(..))
import App

createApp : String -> String -> Cmd Msg
createApp name description =
    Http.post "/api/apps" (App.encode name description) App.decode
        |> sendRequest
        |> Cmd.map NewApp

getApp : String -> Cmd Msg
getApp id =
    Http.get ("/api/apps/" ++ id) App.decode
        |> sendRequest
        |> Cmd.map FetchApp


getApps : Cmd Msg
getApps =
    Http.get "/api/apps" App.decodeList
        |> sendRequest
        |> Cmd.map FetchApps