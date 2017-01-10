module Action exposing (Msg(..))

import Navigation exposing (Location)
import RemoteData exposing (WebData)
import Time exposing (Time)

import App exposing (App)
import Route exposing (Route)

type Msg
    = AppDescription String
    | AppName String
    | FetchApp (WebData App)
    | FetchApps (WebData (List App))
    | ListApps
    | LocationChange Location
    | Navigate Route.Route
    | NewApp (WebData App)
    | SelectApp String
    | SubmitAppForm
    | Tick Time
