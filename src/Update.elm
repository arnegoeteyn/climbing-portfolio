module Update exposing (update)

import Browser
import Browser.Navigation as Nav
import Data exposing (AscentKind(..), ClimbingRouteKind(..), Sector, ascentKindDecoder, climbingRouteKindDecoder, encodedJsonFile, jsonFileDecoder)
import Date
import DatePicker
import Dict exposing (Dict)
import File
import File.Download
import File.Select
import Init exposing (parseUrl)
import Json.Decode exposing (decodeString, maybe)
import Json.Encode exposing (encode)
import Message exposing (ClimbingRouteMsg(..), Item(..), ItemPageMsg(..), Msg(..))
import Model exposing (AppState(..), FormState(..), ItemPageItemForm, Model)
import Set
import Task
import Update.Home
import Update.ItemPage
import Url
import Utilities.ItemPageUtilities as ItemPageUtilities


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
                                    climbingRouteFromForm model form
                            in
                            { model | climbingRoutes = Dict.insert newClimbingRoute.id newClimbingRoute model.climbingRoutes, sectors = modifiedSectors }

                        SectorItem ->
                            let
                                ( newSector, modifiedAreas ) =
                                    sectorFromForm model form
                            in
                            { model | sectors = Dict.insert newSector.id newSector model.sectors, areas = modifiedAreas }

                        AscentItem ->
                            let
                                ( newAscent, modifiedRoutes ) =
                                    ascentFromForm model form
                            in
                            { model | ascents = Dict.insert newAscent.id newAscent model.ascents, climbingRoutes = modifiedRoutes }

                        AreaItem ->
                            let
                                newArea =
                                    areaFromForm model form
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

        ItemPage item itemPageMsg ->
            Update.ItemPage.update itemPageMsg item model


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


getCriteriumValueFromForm : String -> ItemPageItemForm -> Maybe String
getCriteriumValueFromForm key form =
    Dict.get key form.criteria |> Maybe.map .value


climbingRouteFromForm : Model -> ItemPageItemForm -> ( Data.ClimbingRoute, Dict.Dict Int Data.Sector )
climbingRouteFromForm model form =
    let
        newRouteId =
            ItemPageUtilities.getNewIdFromFrom form model.climbingRoutes

        maybeSector =
            ItemPageUtilities.getParentFromForm form model.sectors

        maybeKind =
            getCriteriumValueFromForm "kind" form

        modifiedSectors =
            ItemPageUtilities.modifiedParentCollection newRouteId
                maybeSector
                .routeIds
                (\newIds sector -> { sector | routeIds = newIds })
                model.sectors

        maybeName =
            getCriteriumValueFromForm "name" form

        maybeGrade =
            getCriteriumValueFromForm "grade" form

        maybeComment =
            getCriteriumValueFromForm "comment" form
    in
    ( { id = newRouteId
      , sectorId = Maybe.map .id maybeSector
      , name = Maybe.withDefault "" maybeName
      , grade = Maybe.withDefault "" maybeGrade
      , comment = maybeComment
      , ascentIds = Just Set.empty
      , kind = Maybe.andThen (Result.toMaybe << Json.Decode.decodeString climbingRouteKindDecoder) maybeKind |> Maybe.withDefault Sport
      }
    , modifiedSectors
    )


sectorFromForm : Model -> ItemPageItemForm -> ( Data.Sector, Dict.Dict Int Data.Area )
sectorFromForm model form =
    let
        newSectorId =
            ItemPageUtilities.getNewIdFromFrom form model.sectors

        maybeArea =
            ItemPageUtilities.getParentFromForm form model.areas

        modifiedAreas =
            ItemPageUtilities.modifiedParentCollection newSectorId
                maybeArea
                .sectorIds
                (\newIds area -> { area | sectorIds = newIds })
                model.areas

        maybeName =
            getCriteriumValueFromForm "name" form
    in
    ( { id = newSectorId
      , name = Maybe.withDefault "" maybeName
      , routeIds = Nothing
      , areaId = Maybe.map .id maybeArea
      }
    , modifiedAreas
    )


ascentFromForm : Model -> ItemPageItemForm -> ( Data.Ascent, Dict.Dict Int Data.ClimbingRoute )
ascentFromForm model form =
    let
        newAscentId =
            ItemPageUtilities.getNewIdFromFrom form model.ascents

        maybeRoute =
            ItemPageUtilities.getParentFromForm form model.climbingRoutes

        modifiedRoutes =
            ItemPageUtilities.modifiedParentCollection newAscentId
                maybeRoute
                .ascentIds
                (\newIds route -> { route | ascentIds = newIds })
                model.climbingRoutes

        maybeComment =
            getCriteriumValueFromForm "comment" form

        maybeDate =
            getCriteriumValueFromForm "date" form

        maybeKind =
            getCriteriumValueFromForm "kind" form
    in
    ( { id = newAscentId
      , comment = maybeComment
      , date = maybeDate
      , routeId = Maybe.map .id maybeRoute
      , kind = Maybe.andThen (Result.toMaybe << Json.Decode.decodeString ascentKindDecoder) maybeKind |> Maybe.withDefault Redpoint
      }
    , modifiedRoutes
    )


areaFromForm : Model -> ItemPageItemForm -> Data.Area
areaFromForm model form =
    let
        newAreaId =
            ItemPageUtilities.getNewIdFromFrom form model.areas

        maybeName =
            getCriteriumValueFromForm "name" form

        maybeCountry =
            getCriteriumValueFromForm "country" form
    in
    { id = newAreaId
    , name = Maybe.withDefault "" maybeName
    , country = Maybe.withDefault "" maybeCountry
    , sectorIds = Nothing
    }
