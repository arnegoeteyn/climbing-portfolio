module Utilities.EntityUtilities exposing (..)

import Chart.Attributes exposing (area)
import Data exposing (Area, Ascent, ClimbingRoute, Sector, Trip)
import Date
import Dict
import Message exposing (ItemType(..))
import Model exposing (Model)
import Set exposing (Set)
import Utilities



--| Generic


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

        TripItem ->
            Nothing


getChildren : ItemType -> Int -> Model -> Set Int
getChildren type_ id model =
    Maybe.withDefault Set.empty <|
        case type_ of
            AreaItem ->
                getArea model id |> Maybe.andThen .sectorIds

            SectorItem ->
                getSector model id |> Maybe.andThen .routeIds

            ClimbingRouteItem ->
                getClimbingRoute model id |> Maybe.andThen .ascentIds

            AscentItem ->
                Nothing

            TripItem ->
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

        TripItem ->
            Nothing


getParent : ItemType -> Int -> Model -> Maybe Int
getParent type_ id model =
    case type_ of
        AreaItem ->
            Nothing

        SectorItem ->
            getSector model id |> Maybe.andThen .areaId

        ClimbingRouteItem ->
            getClimbingRoute model id |> Maybe.andThen .sectorId

        AscentItem ->
            getAscent id model |> Maybe.andThen .routeId

        TripItem ->
            Nothing


sortEntityBy : ItemType -> Int -> Model -> String
sortEntityBy type_ id model =
    Maybe.withDefault "" <|
        case type_ of
            AreaItem ->
                getArea model id |> Maybe.map .name

            SectorItem ->
                getSector model id |> Maybe.map .name

            ClimbingRouteItem ->
                getClimbingRoute model id |> Maybe.map (\c -> Utilities.stringFromList [ c.grade, "!", c.name ])

            AscentItem ->
                getAscent id model |> Maybe.andThen (\ascent -> Maybe.map Date.toIsoString ascent.date)

            TripItem ->
                getTrip id model |> Maybe.map (.from >> Date.toIsoString)



--| Acessors


getArea : Model -> Int -> Maybe Area
getArea model id =
    Dict.get id model.areas


getSector : Model -> Int -> Maybe Sector
getSector model id =
    Dict.get id model.sectors


getClimbingRoute : Model -> Int -> Maybe ClimbingRoute
getClimbingRoute model id =
    Dict.get id model.climbingRoutes


getAscent : Int -> Model -> Maybe Ascent
getAscent id model =
    Dict.get id model.ascents


getTrip : Int -> Model -> Maybe Trip
getTrip id model =
    Dict.get id model.trips



--| Filters
-- Elm does not allow custom comparable items so these can't be made a type
-- Elm also doesn't allow constants in branches so it's pretty messy


matchesFilter : ItemType -> Int -> String -> String -> Model -> Bool
matchesFilter type_ id key value model =
    case type_ of
        AreaItem ->
            let
                maybeArea =
                    getArea model id
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
            case getSector model id of
                Nothing ->
                    True

                Just sector ->
                    String.contains (String.toLower value)
                        (String.toLower <|
                            case key of
                                "name" ->
                                    sector.name

                                "area" ->
                                    sector.areaId
                                        |> Maybe.andThen (\areaId -> getArea model areaId)
                                        |> Maybe.map .name
                                        |> Maybe.withDefault ""

                                _ ->
                                    value
                        )

        ClimbingRouteItem ->
            case getClimbingRoute model id of
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
                                        |> Maybe.andThen (\sectorId -> getSector model sectorId)
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
                                    ascent.date |> Maybe.map Date.toIsoString |> Maybe.withDefault ""

                                "kind" ->
                                    Data.ascentKindToString ascent.kind

                                "route" ->
                                    ascent.routeId
                                        |> Maybe.andThen (\routeId -> getClimbingRoute model routeId)
                                        |> Maybe.map .name
                                        |> Maybe.withDefault ""

                                _ ->
                                    value
                        )

        TripItem ->
            True



--| Sector


areaFromSector : Model -> Sector -> Maybe Area
areaFromSector model sector =
    sector.areaId |> Maybe.andThen (getArea model)



--| ClimbingRoute


sectorFromClimbingRoute : Model -> ClimbingRoute -> Maybe Sector
sectorFromClimbingRoute model route =
    route.sectorId |> Maybe.andThen (getSector model)



--| Ascent


climbingRouteFromAscent : Model -> Ascent -> Maybe ClimbingRoute
climbingRouteFromAscent model ascent =
    ascent.routeId |> Maybe.andThen (getClimbingRoute model)



--| Trip


tripTitle : Trip -> String
tripTitle trip =
    Utilities.stringFromList [ Date.toIsoString trip.from, " - ", Date.toIsoString trip.to ]


groupedRoutesFromTrip : Trip -> Model -> List ( String, Int )
groupedRoutesFromTrip trip model =
    ascentsFromTrip trip model
        |> List.map (\item -> getClimbingRoute model (Maybe.withDefault -1 item.routeId))
        |> List.filterMap identity
        |> List.foldl
            (\value acc ->
                Dict.insert value.grade
                    (Dict.get value.grade acc |> Maybe.withDefault 0 |> (+) 1)
                    acc
            )
            Dict.empty
        |> Dict.toList
        |> Utilities.sortDescending


ascentsFromTrip : Trip -> Model -> List Ascent
ascentsFromTrip trip model =
    Dict.filter
        (\_ ascent -> Maybe.map (Date.isBetween trip.from trip.to) ascent.date |> Maybe.withDefault False)
        model.ascents
        |> Dict.toList
        |> List.map Tuple.second


areasFromTrip : Model -> Trip -> List Area
areasFromTrip model trip =
    ascentsFromTrip trip model
        |> List.map (climbingRouteFromAscent model)
        |> List.map (Maybe.andThen (sectorFromClimbingRoute model))
        |> List.map (Maybe.andThen (\sector -> getParent SectorItem sector.id model))
        |> List.filterMap identity
        |> Set.fromList
        |> Set.toList
        |> List.map (getArea model)
        |> List.filterMap identity
