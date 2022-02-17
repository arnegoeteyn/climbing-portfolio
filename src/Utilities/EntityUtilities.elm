module Utilities.EntityUtilities exposing (..)

import Data exposing (Area, Ascent, ClimbingRoute, Sector)
import Dict
import Message exposing (ItemType(..))
import Model exposing (Model)
import Set exposing (Set)


getChildType : ItemType -> Maybe ItemType
getChildType type_ =
    case type_ of
        AreaItem ->
            Just SectorItem

        SectorItem ->
            Just ClimbingRouteItem

        ClimbingRouteItem ->
            Just AscentItem

        AscentItem ->
            Nothing


getChildren : ItemType -> Int -> Model -> Set Int
getChildren type_ id model =
    Maybe.withDefault Set.empty <|
        case type_ of
            AreaItem ->
                getArea id model |> Maybe.andThen .sectorIds

            SectorItem ->
                getSector id model |> Maybe.andThen .routeIds

            ClimbingRouteItem ->
                getClimbingRoute id model |> Maybe.andThen .ascentIds

            AscentItem ->
                Nothing


getParentType : ItemType -> Maybe ItemType
getParentType type_ =
    case type_ of
        AreaItem ->
            Nothing

        SectorItem ->
            Just AreaItem

        ClimbingRouteItem ->
            Just SectorItem

        AscentItem ->
            Just ClimbingRouteItem


getParent : ItemType -> Int -> Model -> Maybe Int
getParent type_ id model =
    case type_ of
        AreaItem ->
            Nothing

        SectorItem ->
            getSector id model |> Maybe.andThen .areaId

        ClimbingRouteItem ->
            getClimbingRoute id model |> Maybe.andThen .sectorId

        AscentItem ->
            getAscent id model |> Maybe.andThen .routeId


getArea : Int -> Model -> Maybe Area
getArea id model =
    Dict.get id model.areas


getSector : Int -> Model -> Maybe Sector
getSector id model =
    Dict.get id model.sectors


getClimbingRoute : Int -> Model -> Maybe ClimbingRoute
getClimbingRoute id model =
    Dict.get id model.climbingRoutes


getAscent : Int -> Model -> Maybe Ascent
getAscent id model =
    Dict.get id model.ascents
