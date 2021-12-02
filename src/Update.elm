module Update exposing (update)

import Browser
import Browser.Navigation as Nav
import Data exposing (encodedJsonFile, jsonFileDecoder)
import Date
import DatePicker
import Dict
import File
import File.Download
import File.Select
import Init exposing (parseUrl)
import Json.Decode exposing (decodeString, maybe)
import Json.Encode exposing (encode)
import Message exposing (ClimbingRouteMsg(..), Msg(..))
import Model exposing (AppState(..), Model)
import Task exposing (Task)
import Update.ClimbingRoute exposing (updateClimbingRoute)
import Url
import Utilities exposing (newId)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedUrl url ->
            ( { model | route = parseUrl url }, Cmd.none )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        JsonRequested ->
            ( model, File.Select.file [ "application/json" ] JsonSelected )

        JsonSelected file ->
            ( model, Task.perform JsonLoaded (File.toString file) )

        JsonLoaded content ->
            let
                result =
                    decodeString jsonFileDecoder content
            in
            case result of
                Ok file ->
                    ( { model | appState = Ready, climbingRoutes = file.climbingRoutes, ascents = file.ascents }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ExportRequested ->
            let
                result =
                    encode 4 <| encodedJsonFile { climbingRoutes = model.climbingRoutes, ascents = model.ascents }
            in
            ( model, File.Download.string "result.json" "application/json" result )

        SaveRouteRequested ->
            case model.climbingRoutesModel.form of
                Just form ->
                    let
                        newClimbingRoute =
                            { name = form.name, grade = form.grade, description = Just "dit is nieuw", id = newId model.climbingRoutes, ascentIds = Just [] }
                    in
                    ( { model | climbingRoutes = Dict.insert newClimbingRoute.id newClimbingRoute model.climbingRoutes }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ClimbingRoute climbingRouteMsg ->
            updateClimbingRoute climbingRouteMsg model

        Home homeMsg ->
            ( model, Cmd.none )

        ToDatePicker subMsg ->
            let
                climbingRoutesModel =
                    model.climbingRoutesModel

                ( newDatePicker, event ) =
                    DatePicker.update DatePicker.defaultSettings subMsg climbingRoutesModel.datePicker

                maybeResult =
                    Maybe.andThen
                        (\selectedRoute ->
                            case event of
                                DatePicker.Picked date ->
                                    let
                                        newAscentId =
                                            newId model.ascents

                                        modifiedRoute =
                                            { selectedRoute | ascentIds = Just <| newAscentId :: Maybe.withDefault [] selectedRoute.ascentIds }

                                        newRoutes =
                                            Dict.insert selectedRoute.id modifiedRoute model.climbingRoutes

                                        newAscent =
                                            { id = newAscentId, routeId = selectedRoute.id, date = Date.toIsoString date }

                                        newAscents =
                                            Dict.insert newAscentId newAscent model.ascents
                                    in
                                    Just
                                        { newRoutes = newRoutes
                                        , newAscents = newAscents
                                        , newClimbingRoutesModel = { climbingRoutesModel | selectedRoute = Just modifiedRoute, date = Nothing }
                                        }

                                _ ->
                                    Nothing
                        )
                        model.climbingRoutesModel.selectedRoute

                newModel =
                    Maybe.andThen (\result -> Just { model | ascents = result.newAscents, climbingRoutes = result.newRoutes }) maybeResult
                        |> Maybe.withDefault model

                newClimbingRoutesModel =
                    Maybe.andThen (Just << .newClimbingRoutesModel) maybeResult
                        |> Maybe.withDefault climbingRoutesModel
                        |> (\c -> { c | datePicker = newDatePicker })
            in
            ( { newModel | climbingRoutesModel = newClimbingRoutesModel }, Cmd.none )
