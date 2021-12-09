module Utilities.ItemPageUtilities exposing (..)

import Data exposing (Area, Ascent, ClimbingRoute, ItemPageItem, Sector)
import Dict exposing (Dict)
import Init exposing (areaForm, ascentForm, climbingRouteForm, sectorForm)
import Message exposing (Item(..), ItemRelation)
import Model exposing (Criterium, ItemPageItemForm, ItemPageModel, Model)


getModelFromItem : Item -> Model -> ItemPageModel
getModelFromItem item model =
    case item of
        ClimbingRouteItem ->
            model.climbingRoutesModel

        SectorItem ->
            model.sectorsModel

        AscentItem ->
            model.ascentsModel

        AreaItem ->
            model.areasModel


getCriteriaForItem : Item -> ItemPageItemForm
getCriteriaForItem item =
    case item of
        ClimbingRouteItem ->
            climbingRouteForm

        AscentItem ->
            ascentForm

        SectorItem ->
            sectorForm

        AreaItem ->
            areaForm


setItemPageModel : Item -> ItemPageModel -> Model -> Model
setItemPageModel item itemPageModel model =
    case item of
        ClimbingRouteItem ->
            { model | climbingRoutesModel = itemPageModel }

        SectorItem ->
            { model | sectorsModel = itemPageModel }

        AscentItem ->
            { model | ascentsModel = itemPageModel }

        AreaItem ->
            { model | areasModel = itemPageModel }


getDataFromItem : Item -> Model -> Dict Int ItemPageItem
getDataFromItem item model =
    case item of
        ClimbingRouteItem ->
            Dict.map toClimbingRouteItem model.climbingRoutes

        AscentItem ->
            Dict.map toAscentItem model.ascents

        SectorItem ->
            Dict.map toSectorItem model.sectors

        AreaItem ->
            Dict.map toAreaItem model.areas


getRelationFromItem : Item -> ItemRelation
getRelationFromItem item =
    case item of
        ClimbingRouteItem ->
            Init.climbingRouteRelations

        AscentItem ->
            Init.ascentRelations

        SectorItem ->
            Init.sectorRelations

        AreaItem ->
            Init.areaRelations


getCriteriaFromItem : Int -> Item -> Model -> Dict String Criterium
getCriteriaFromItem requestId itemType model =
    Maybe.withDefault Dict.empty <|
        case itemType of
            ClimbingRouteItem ->
                Dict.get requestId model.climbingRoutes
                    |> Maybe.map toClimbingRouteFormCriteria

            AscentItem ->
                Dict.get requestId model.ascents
                    |> Maybe.map toAscentFormCriteria

            AreaItem ->
                Dict.get requestId model.areas
                    |> Maybe.map toAreaFormCriteria

            SectorItem ->
                Dict.get requestId model.sectors
                    |> Maybe.map toSectorFormCriteria


toClimbingRouteItem : Int -> ClimbingRoute -> ItemPageItem
toClimbingRouteItem _ climbingRoute =
    { cardHeader =
        List.foldr (++) "" <|
            [ climbingRoute.name, " [", climbingRoute.grade, "]" ]
    , identifier = climbingRoute.name
    , cardDescription = climbingRoute.description
    , id = climbingRoute.id
    , parentId = climbingRoute.sectorId
    , childIds = climbingRoute.ascentIds
    }


toClimbingRouteFormCriteria : ClimbingRoute -> Dict String Criterium
toClimbingRouteFormCriteria route =
    Dict.fromList
        [ ( "_parentId", { value = route.sectorId |> Maybe.map String.fromInt |> Maybe.withDefault "", label = "_parentId" } )
        , ( "name", { value = route.name, label = "name" } )
        , ( "grade", { value = route.grade, label = "grade" } )
        , ( "description", { value = Maybe.withDefault "" route.description, label = "description" } )
        ]


toSectorItem : Int -> Sector -> ItemPageItem
toSectorItem _ sector =
    { cardHeader = sector.name
    , identifier = sector.name
    , id = sector.id
    , cardDescription = Nothing
    , parentId = sector.areaId
    , childIds = sector.routeIds
    }


toSectorFormCriteria : Sector -> Dict String Criterium
toSectorFormCriteria sector =
    Dict.empty


toAreaItem : Int -> Area -> ItemPageItem
toAreaItem _ area =
    { cardHeader = area.name
    , identifier = area.name
    , id = area.id
    , cardDescription = Just area.country
    , parentId = Nothing
    , childIds = area.sectorIds
    }


toAreaFormCriteria : Area -> Dict String Criterium
toAreaFormCriteria area =
    Dict.empty


toAscentItem : Int -> Ascent -> ItemPageItem
toAscentItem _ ascent =
    { cardHeader = ascent.date
    , identifier = ascent.date
    , cardDescription = Nothing
    , id = ascent.id
    , parentId = Just ascent.routeId
    , childIds = Nothing
    }


toAscentFormCriteria : Ascent -> Dict String Criterium
toAscentFormCriteria ascent =
    Dict.empty
