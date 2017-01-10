module Route exposing (..)

import Navigation exposing (Location, newUrl)
import UrlParser exposing (Parser, (</>), map, oneOf, parsePath, top, s, string)

-- MODEL

type Route
    = App String
    | Apps
    | Dashboard
    | Members


-- HELPER


construct : Route -> String
construct route =
    case route of
        App id ->
            "/apps/" ++ id

        Apps ->
            "/apps"

        Members ->
            "/members"

        Dashboard ->
            "/"


navigate : Route -> Cmd msg
navigate route =
    newUrl (construct route)


parse : Location -> Maybe Route
parse location =
    parsePath routes location


routes : Parser (Route -> a) a
routes =
    oneOf
        [ map Dashboard top
        , map App (s "apps" </> string)
        , map Apps (s "apps")
        , map Members (s "members")
        ]
