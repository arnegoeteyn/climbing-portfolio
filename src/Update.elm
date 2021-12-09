module Update exposing (update)

import Browser
import Browser.Navigation as Nav
import Data exposing (encodedJsonFile, jsonFileDecoder)
import Dict
import File
import File.Download
import File.Select
import Init exposing (parseUrl)
import Json.Decode exposing (decodeString, maybe)
import Json.Encode exposing (encode)
import Message exposing (ClimbingRouteMsg(..), Item(..), Msg(..))
import Model exposing (AppState(..), FormState(..), ItemPageItemForm, Model)
import Task
import Update.ItemPage
import Url
import Utilities exposing (newId)
import Utilities.ItemPageUtilities exposing (getModelFromItem)


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
                        , areas = file.areas
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ExportRequested ->
            let
                result =
                    encode 4 <| encodedJsonFile { climbingRoutes = model.climbingRoutes, ascents = model.ascents, sectors = model.sectors, areas = model.areas }
            in
            ( model, File.Download.string "result.json" "application/json" result )

        ClimbingRoute climbingRouteMsg ->
            ( model, Cmd.none )

        Sector sectorMsg ->
            ( model, Cmd.none )

        Home homeMsg ->
            ( model, Cmd.none )

        SaveItemRequested item ->
            let
                itemPageModel =
                    getModelFromItem item model

                form =
                    itemPageModel.form

                newModel =
                    case item of
                        ClimbingRouteItem ->
                            let
                                ( newClimbingRoute, modifiedSectors ) =
                                    climbingRouteFromForm model form
                            in
                            { model | climbingRoutes = Dict.insert newClimbingRoute.id newClimbingRoute model.climbingRoutes, sectors = modifiedSectors }

                        _ ->
                            let
                                newSector =
                                    sectorFromForm model form
                            in
                            { model | sectors = Dict.insert newSector.id newSector model.sectors }
            in
            ( newModel, Cmd.none )

        ToDatePicker subMsg ->
            ( model, Cmd.none )

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


getCriteriumValueFromForm : String -> ItemPageItemForm -> Maybe String
getCriteriumValueFromForm key form =
    Dict.get key form.criteria |> Maybe.map .value


climbingRouteFromForm : Model -> ItemPageItemForm -> ( Data.ClimbingRoute, Dict.Dict Int Data.Sector )
climbingRouteFromForm model form =
    let
        newRouteId =
            case form.formState of
                Update id ->
                    id

                _ ->
                    newId model.climbingRoutes

        maybeSector =
            form.parentId
                |> Maybe.andThen String.toInt
                |> Maybe.andThen (\id -> Dict.get id model.sectors)

        maybeName =
            getCriteriumValueFromForm "name" form

        maybeGrade =
            getCriteriumValueFromForm "grade" form

        maybeDescription =
            getCriteriumValueFromForm "description" form

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
    ( { id = newRouteId
      , sectorId = Maybe.map .id maybeSector
      , name = Maybe.withDefault "" maybeName
      , grade = Maybe.withDefault "" maybeGrade
      , description = maybeDescription
      , ascentIds = Just []
      }
    , modifiedSectors
    )


sectorFromForm : Model -> ItemPageItemForm -> Data.Sector
sectorFromForm model form =
    let
        newSectorId =
            newId model.sectors

        maybeName =
            getCriteriumValueFromForm "name" form

        maybeArea =
            form.parentId
                |> Maybe.andThen String.toInt
                |> Maybe.andThen (\id -> Dict.get id model.areas)
    in
    { id = newSectorId
    , name = Maybe.withDefault "" maybeName
    , routeIds = Nothing
    , areaId = Maybe.map .id maybeArea
    }
