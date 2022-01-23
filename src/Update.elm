module Update exposing (update)

import Browser
import Browser.Navigation as Nav
import Data exposing (AscentKind(..), ClimbingRouteKind(..), encodedJsonFile, jsonFileDecoder)
import Date
import DatePicker
import Dict exposing (Dict)
import File
import File.Download
import File.Select
import Init exposing (parseUrl)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import Message exposing (ClimbingRouteMsg(..), Item(..), ItemPageMsg(..), Msg(..))
import Model exposing (AppState(..), FormState(..), Model)
import Set
import Task
import Update.Home
import Update.ItemPage
import Url
import Utilities.ItemFormUtilities as ItemFormUtilities
import Utilities.ItemPageUtilities as ItemPageUtilities


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Dummy ->
            ( model, Cmd.none )

        ChangedUrl url ->
            let
                parsedUrl =
                    parseUrl url
            in
            ( { model | route = parsedUrl, url = url }, Cmd.none )

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

        ItemPage item itemPageMsg ->
            Update.ItemPage.update itemPageMsg item model

        Home homeMsg ->
            Update.Home.update homeMsg model

        DeleteItem item id ->
            let
                l =
                    Set.fromList [ id ]

                updateData =
                    case item of
                        AreaItem ->
                            deleteArea model l

                        SectorItem ->
                            deleteSector model l

                        ClimbingRouteItem ->
                            deleteClimbingRoute model l

                        AscentItem ->
                            deleteAscent model l
            in
            ( { model
                | areas = updateData.areas
                , sectors = updateData.sectors
                , climbingRoutes = updateData.climbingRoutes
                , ascents = updateData.ascents
              }
            , Cmd.none
            )

        SaveItemRequested item ->
            let
                itemPageModel =
                    ItemPageUtilities.getModelFromItem item model

                form =
                    itemPageModel.form

                newModel =
                    case item of
                        ClimbingRouteItem ->
                            let
                                ( newClimbingRoute, modifiedSectors ) =
                                    ItemFormUtilities.climbingRouteFromForm model form
                            in
                            { model | climbingRoutes = Dict.insert newClimbingRoute.id newClimbingRoute model.climbingRoutes, sectors = modifiedSectors }

                        SectorItem ->
                            let
                                ( newSector, modifiedAreas ) =
                                    ItemFormUtilities.sectorFromForm model form
                            in
                            { model | sectors = Dict.insert newSector.id newSector model.sectors, areas = modifiedAreas }

                        AscentItem ->
                            let
                                ( newAscent, modifiedRoutes ) =
                                    ItemFormUtilities.ascentFromForm model form
                            in
                            { model | ascents = Dict.insert newAscent.id newAscent model.ascents, climbingRoutes = modifiedRoutes }

                        AreaItem ->
                            let
                                newArea =
                                    ItemFormUtilities.areaFromForm model form
                            in
                            { model | areas = Dict.insert newArea.id newArea model.areas }
            in
            ( newModel, Cmd.none )

        ToDatePicker item criterium subMsg ->
            -- todo rewrite this to be able to work with multiple datepickers in the application
            let
                ( newDatePicker, event ) =
                    DatePicker.update DatePicker.defaultSettings subMsg model.datePicker

                ( newModel, newCmd ) =
                    case event of
                        DatePicker.Picked date ->
                            Update.ItemPage.update (FormUpdateMessage (Message.UpdateKey criterium (Date.toIsoString date))) item model

                        _ ->
                            ( model, Cmd.none )
            in
            ( { newModel | datePicker = newDatePicker }, newCmd )


type alias ModelData =
    { areas : Dict Int Data.Area
    , sectors : Dict Int Data.Sector
    , climbingRoutes : Dict Int Data.ClimbingRoute
    , ascents : Dict Int Data.Ascent
    }


deleteArea : Model -> Set.Set Int -> ModelData
deleteArea model ids =
    let
        newAreas =
            Dict.filter (\_ value -> not <| Set.member value.id ids) model.areas

        sectorIds =
            Set.foldl
                (\id curr -> Set.union curr (Dict.get id model.areas |> Maybe.andThen .sectorIds |> Maybe.withDefault Set.empty))
                Set.empty
                ids

        sectorsUpdate =
            deleteSector model sectorIds
    in
    { areas = newAreas, sectors = sectorsUpdate.sectors, climbingRoutes = sectorsUpdate.climbingRoutes, ascents = sectorsUpdate.ascents }


deleteSector : Model -> Set.Set Int -> ModelData
deleteSector model ids =
    let
        newSectors =
            Dict.filter (\_ value -> not <| Set.member value.id ids) model.sectors

        routeIds =
            Set.foldl
                (\id curr -> Set.union curr (Dict.get id model.sectors |> Maybe.andThen .routeIds |> Maybe.withDefault Set.empty))
                Set.empty
                ids

        climbingRouteUpdate =
            deleteClimbingRoute model routeIds
    in
    { areas = model.areas, sectors = newSectors, climbingRoutes = climbingRouteUpdate.climbingRoutes, ascents = climbingRouteUpdate.ascents }


deleteClimbingRoute : Model -> Set.Set Int -> ModelData
deleteClimbingRoute model ids =
    let
        newRoutes =
            Dict.filter (\_ value -> not <| Set.member value.id ids) model.climbingRoutes

        ascentIds =
            Set.foldl
                (\id curr -> Set.union curr (Dict.get id model.climbingRoutes |> Maybe.andThen .ascentIds |> Maybe.withDefault Set.empty))
                Set.empty
                ids

        ascentUpdate =
            deleteAscent model
                ascentIds
    in
    { areas = model.areas, sectors = model.sectors, climbingRoutes = newRoutes, ascents = ascentUpdate.ascents }


deleteAscent : Model -> Set.Set Int -> ModelData
deleteAscent model ids =
    let
        newAscents =
            Dict.filter (\_ value -> not <| Set.member value.id ids) model.ascents
    in
    { areas = model.areas, sectors = model.sectors, climbingRoutes = model.climbingRoutes, ascents = newAscents }
