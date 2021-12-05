module Utilities.ItemPageUtilities exposing (..)

import Data exposing (Ascent, ClimbingRoute, ItemPageItem, Sector)
import Dict exposing (Dict)
import Init exposing (ascentForm, climbingRouteForm, sectorForm)
import Message exposing (Item(..), ItemRelation)
import Model exposing (ItemPageItemForm, ItemPageModel, Model)


getModelFromItem : Item -> Model -> ItemPageModel
getModelFromItem item model =
    case item of
        ClimbingRouteItem ->
            model.climbingRoutesModel

        SectorItem ->
            model.sectorsModel

        AscentItem ->
            model.ascentsModel


getCriteriaFromItem : Item -> ItemPageItemForm
getCriteriaFromItem item =
    case item of
        ClimbingRouteItem ->
            climbingRouteForm

        AscentItem ->
            ascentForm

        SectorItem ->
            sectorForm


setItemPageModel : Item -> ItemPageModel -> Model -> Model
setItemPageModel item itemPageModel model =
    case item of
        ClimbingRouteItem ->
            { model | climbingRoutesModel = itemPageModel }

        SectorItem ->
            { model | sectorsModel = itemPageModel }

        AscentItem ->
            { model | ascentsModel = itemPageModel }


getDataFromItem : Item -> Model -> Dict Int ItemPageItem
getDataFromItem item model =
    case item of
        ClimbingRouteItem ->
            Dict.map toClimbingRouteItem model.climbingRoutes

        AscentItem ->
            Dict.map toAscentItem model.ascents

        SectorItem ->
            Dict.map toSectorItem model.sectors


getRelationFromItem : Item -> ItemRelation
getRelationFromItem item =
    case item of
        ClimbingRouteItem ->
            Init.climbingRouteRelations

        AscentItem ->
            Init.ascentRelations

        SectorItem ->
            Init.sectorRelations


toClimbingRouteItem : Int -> ClimbingRoute -> ItemPageItem
toClimbingRouteItem _ climbingRoute =
    { cardHeader =
        List.foldr (++) "" <|
            [ climbingRoute.name, " [", climbingRoute.grade, "]" ]
    , identifier = climbingRoute.name
    , cardDescription = climbingRoute.description
    , id = climbingRoute.id
    , parentId = climbingRoute.sectorId
    }


toSectorItem : Int -> Sector -> ItemPageItem
toSectorItem _ sector =
    { cardHeader = sector.name
    , identifier = sector.name
    , id = sector.id
    , cardDescription = Nothing
    , parentId = Nothing
    }


toAscentItem : Int -> Ascent -> ItemPageItem
toAscentItem _ ascent =
    { cardHeader = ascent.date
    , identifier = ascent.date
    , cardDescription = Nothing
    , id = ascent.id
    , parentId = Just ascent.routeId
    }
