module Utilities.EntityUtilities exposing (..)

import Data exposing (Area, Ascent, ClimbingRoute, Sector)
import Dict
import Message exposing (ItemType(..))
import Model exposing (Model)
import Set exposing (Set)
import Utilities


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


sortEntityBy : ItemType -> Int -> Model -> String
sortEntityBy type_ id model =
    Maybe.withDefault "" <|
        case type_ of
            AreaItem ->
                getArea id model |> Maybe.map .name

            SectorItem ->
                getSector id model |> Maybe.map .name

            ClimbingRouteItem ->
                getClimbingRoute id model |> Maybe.map (\c -> Utilities.stringFromList [ c.grade, "!", c.name ])

            AscentItem ->
                getAscent id model |> Maybe.andThen .date



-- Elm does not allow custom comparable items so these can't be made a type
-- Elm also doesn't allow constants in branches so it's pretty messy


matchesFilter : ItemType -> Int -> String -> String -> Model -> Bool
matchesFilter type_ id key value model =
    case type_ of
        AreaItem ->
            let
                maybeArea =
                    getArea id model
            in
            case maybeArea of
                Nothing ->
                    True

                Just area ->
                    String.contains (String.toLower value)
                        (String.toLower <|
                            case key of
                                "name" ->
                                    area.name

                                "country" ->
                                    area.country

                                _ ->
                                    value
                        )

        SectorItem ->
            case getSector id model of
                Nothing ->
                    True

                Just sector ->
                    String.contains (String.toLower value)
                        (String.toLower <|
                            case key of
                                "name" ->
                                    sector.name

                                "sector" ->
                                    sector.areaId
                                        |> Maybe.andThen (\areaId -> getSector areaId model)
                                        |> Maybe.map .name
                                        |> Maybe.withDefault ""

                                _ ->
                                    value
                        )

        ClimbingRouteItem ->
            case getClimbingRoute id model of
                Nothing ->
                    True

                Just climbingRoute ->
                    String.contains (String.toLower value)
                        (String.toLower <|
                            case key of
                                "name" ->
                                    climbingRoute.name

                                "grade" ->
                                    climbingRoute.grade

                                "sector" ->
                                    climbingRoute.sectorId
                                        |> Maybe.andThen (\sectorId -> getSector sectorId model)
                                        |> Maybe.map .name
                                        |> Maybe.withDefault ""

                                "kind" ->
                                    Data.climbingRouteKindToString climbingRoute.kind

                                _ ->
                                    value
                        )

        AscentItem ->
            case getAscent id model of
                Nothing ->
                    True

                Just ascent ->
                    String.contains (String.toLower value)
                        (String.toLower <|
                            case key of
                                "date" ->
                                    ascent.date |> Maybe.withDefault ""

                                "kind" ->
                                    Data.ascentKindToString ascent.kind

                                "route" ->
                                    ascent.routeId
                                        |> Maybe.andThen (\routeId -> getClimbingRoute routeId model)
                                        |> Maybe.map .name
                                        |> Maybe.withDefault ""

                                _ ->
                                    value
                        )
