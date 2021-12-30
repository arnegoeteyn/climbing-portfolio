module Update exposing (update)

import Browser
import Browser.Navigation as Nav
import Data exposing (encodedJsonFile, jsonFileDecoder)
import Date
import DatePicker exposing (DatePicker)
import Dict
import File
import File.Download
import File.Select
import Init exposing (parseUrl)
import Json.Decode exposing (decodeString, maybe)
import Json.Encode exposing (encode)
import Message exposing (ClimbingRouteMsg(..), CriteriumUpdate, Item(..), ItemPageMsg(..), Msg(..))
import Model exposing (AppState(..), FormState(..), ItemPageItemForm, Model)
import Set
import Task
import Update.ItemPage
import Url
import Utilities exposing (newId)
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
            ( model, Cmd.none )

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

        maybeDescription =
            getCriteriumValueFromForm "description" form
    in
    ( { id = newRouteId
      , sectorId = Maybe.map .id maybeSector
      , name = Maybe.withDefault "" maybeName
      , grade = Maybe.withDefault "" maybeGrade
      , description = maybeDescription
      , ascentIds = Just Set.empty
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

        maybeDescription =
            getCriteriumValueFromForm "description" form

        maybeDate =
            getCriteriumValueFromForm "date" form
    in
    ( { id = newAscentId
      , description = maybeDescription
      , date = maybeDate
      , routeId = Maybe.map .id maybeRoute
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
