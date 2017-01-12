module App.Model exposing (App, decode, decodeList, encode)

import Http
import Json.Decode as Decode
import Json.Encode as Encode

-- MODEL


type alias App =
    { backend_token : String
    , description : String
    , enabled : Bool
    , id : String
    , name : String
    , token : String
    }


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
    Encode.object
        [ ( "name", Encode.string name )
        , ( "description", Encode.string description )
        ]
        |> Http.jsonBody

