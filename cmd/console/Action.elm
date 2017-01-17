module Action exposing (Msg(..))

import Navigation exposing (Location)
import RemoteData exposing (WebData)
import Time exposing (Time)

import App.Model exposing (App)
import Route exposing (Route)

type Msg
    = AppFormBlur String
    | AppFormClear
    | AppFormFocus String
    | AppFormSubmit
    | AppFormUpdate String String
    | FetchApp (WebData App)
    | FetchApps (WebData (List App))
    | ListApps
    | LocationChange Location
    | Navigate Route
    | NewApp (WebData App)
    | SelectApp String
    | Tick Time
