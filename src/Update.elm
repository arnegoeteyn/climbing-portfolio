module Update exposing (update)

import Browser
import Browser.Navigation as Nav
import Data exposing (climbingRouteFromParameters, encodedJsonFile, jsonFileDecoder)
import Dict
import File
import File.Download
import File.Select
import Init exposing (parseUrl)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import Message exposing (ClimbingRouteMsg(..), Msg(..))
import Model exposing (AppState(..), ItemPageItemForm, Model)
import Task
import Update.ClimbingRoute exposing (updateClimbingRoute)
import Update.ItemPage
import Url
import Utilities exposing (newId)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Dummy ->
            ( model, Cmd.none )

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
                    ( { model
                        | appState = Ready
                        , climbingRoutes = file.climbingRoutes
                        , ascents = file.ascents
                        , sectors = file.sectors
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ExportRequested ->
            let
                result =
                    encode 4 <| encodedJsonFile { climbingRoutes = model.climbingRoutes, ascents = model.ascents, sectors = model.sectors }
            in
            ( model, File.Download.string "result.json" "application/json" result )

        ClimbingRoute climbingRouteMsg ->
            updateClimbingRoute climbingRouteMsg model

        Sector sectorMsg ->
            ( model, Cmd.none )

        Home homeMsg ->
            ( model, Cmd.none )

        SaveRouteRequested ->
            case model.climbingRoutesModel.form of
                Just form ->
                    let
                        ( newClimbingRoute, modifiedSectors ) =
                            climbingRouteFromForm model form
                    in
                    ( { model | climbingRoutes = Dict.insert newClimbingRoute.id newClimbingRoute model.climbingRoutes, sectors = modifiedSectors }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        -- ToDatePicker subMsg ->
        --     let
        --         climbingRoutesModel =
        --             model.climbingRoutesModel
        --         ( newDatePicker, event ) =
        --             DatePicker.update DatePicker.defaultSettings subMsg climbingRoutesModel.datePicker
        --         maybeResult =
        --             Maybe.andThen
        --                 (\selectedRoute ->
        --                     case event of
        --                         DatePicker.Picked date ->
        --                             let
        --                                 newAscentId =
        --                                     newId model.ascents
        --                                 modifiedRoute =
        --                                     { selectedRoute | ascentIds = Just <| newAscentId :: Maybe.withDefault [] selectedRoute.ascentIds }
        --                                 newRoutes =
        --                                     Dict.insert selectedRoute.id modifiedRoute model.climbingRoutes
        --                                 newAscent =
        --                                     { id = newAscentId, routeId = selectedRoute.id, date = Date.toIsoString date }
        --                                 newAscents =
        --                                     Dict.insert newAscentId newAscent model.ascents
        --                             in
        --                             Just
        --                                 { newRoutes = newRoutes
        --                                 , newAscents = newAscents
        --                                 , newClimbingRoutesModel = { climbingRoutesModel | selectedRoute = Just modifiedRoute, date = Nothing }
        --                                 }
        --                         _ ->
        --                             Nothing
        --                 )
        --                 model.climbingRoutesModel.selectedRoute
        --         newModel =
        --             Maybe.andThen (\result -> Just { model | ascents = result.newAscents, climbingRoutes = result.newRoutes }) maybeResult
        --                 |> Maybe.withDefault model
        --         newClimbingRoutesModel =
        --             Maybe.andThen (Just << .newClimbingRoutesModel) maybeResult
        --                 |> Maybe.withDefault climbingRoutesModel
        --                 |> (\c -> { c | datePicker = newDatePicker })
        --     in
        --     ( { newModel | climbingRoutesModel = newClimbingRoutesModel }, Cmd.none )
        ItemPage item itemPageMsg ->
            Update.ItemPage.update itemPageMsg item model

        _ ->
            ( model, Cmd.none )


getCriteriumValueFromForm : String -> ItemPageItemForm -> Maybe String
getCriteriumValueFromForm key form =
    Dict.get key form.criteria |> Maybe.map .value


climbingRouteFromForm : Model -> ItemPageItemForm -> ( Data.ClimbingRoute, Dict.Dict Int Data.Sector )
climbingRouteFromForm model form =
    let
        newRouteId =
            newId model.climbingRoutes

        maybeSector =
            Just "1"
                |> Maybe.andThen String.toInt
                |> Maybe.andThen (\id -> Dict.get id model.sectors)

        maybeName =
            getCriteriumValueFromForm "name" form

        maybeGrade =
            getCriteriumValueFromForm "grade" form

        newRouteIds =
            newRouteId
                :: (maybeSector
                        |> Maybe.andThen .routeIds
                        |> Maybe.withDefault []
                   )

        modifiedSectors =
            maybeSector
                |> Maybe.map (\sector -> { sector | routeIds = Just newRouteIds })
                |> Maybe.map (\sector -> Dict.insert sector.id sector model.sectors)
                |> Maybe.withDefault model.sectors
    in
    ( { id = newId model.climbingRoutes
      , sectorId = Maybe.map .id maybeSector
      , name = Maybe.withDefault "" maybeName
      , grade = Maybe.withDefault "" maybeGrade
      , description = Nothing
      , ascentIds = Just []
      }
    , modifiedSectors
    )
