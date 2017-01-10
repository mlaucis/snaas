module Update exposing (update)

import Action exposing (Msg(..))
import Api exposing (createApp)
import Model exposing (Flags, Model, init)
import RemoteData exposing (RemoteData(Loading, NotAsked), WebData)
import Route

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AppDescription description ->
            ( { model | appDescription = description }, Cmd.none )

        AppName name ->
            ( { model | appName = name }, Cmd.none )

        FetchApp response ->
            ( { model | app = response }, Cmd.none )

        FetchApps response ->
            ( { model | app = NotAsked, apps = response }, Cmd.none )

        ListApps ->
            ( model, Cmd.map LocationChange (Route.navigate Route.Apps) )

        LocationChange location ->
            init (Flags model.zone) location

        Navigate route ->
            ( model, Cmd.map LocationChange (Route.navigate route) )

        NewApp response ->
            ( { model | apps = (appendWebData model.apps response), newApp = NotAsked }, Cmd.none )

        SelectApp id ->
            ( model, Cmd.map LocationChange (Route.navigate (Route.App id)) )

        SubmitAppForm ->
            ( { model | newApp = Loading }, createApp model.appName model.appDescription )

        Tick time ->
            let
                startTime =
                    if model.startTime == 0 then
                        time
                    else
                        model.startTime
            in
                ( { model | startTime = startTime, time = time }, Cmd.none )

appendWebData : WebData (List a) -> WebData a -> WebData (List a)
appendWebData list single =
    case (RemoteData.toMaybe single) of
        Nothing ->
            list

        Just a ->
            case (RemoteData.toMaybe list) of
                Nothing ->
                    RemoteData.succeed [ a ]

                Just list ->
                    RemoteData.succeed (list ++ [ a ])