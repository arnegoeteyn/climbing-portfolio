module Utilities.Entity2Utilities exposing (..)

import Data exposing (Area, Ascent, ClimbingRoute, Sector)
import Dict
import Model exposing (Model)
import Set
import Utilities



--| Ascent


ascentFromId : Model -> Int -> Maybe Ascent
ascentFromId model id =
    Dict.get id model.ascents



--| ClimbingRoute


getSector : Model -> ClimbingRoute -> Maybe Sector
getSector model climbingRoute =
    Maybe.andThen
        (\id -> Dict.get id model.sectors)
        climbingRoute.sectorId


getAscents : Model -> ClimbingRoute -> List Ascent
getAscents model route =
    Set.toList route.ascentIds
        |> List.map (ascentFromId model)
        |> List.filterMap identity


sortByGrade : List ClimbingRoute -> List ClimbingRoute
sortByGrade =
    Utilities.sortByDescending .grade



--| Sector


sectorFromId : Model -> Int -> Maybe Sector
sectorFromId model id =
    Dict.get id model.sectors


getArea : Model -> Sector -> Maybe Area
getArea model sector =
    Maybe.andThen (areaFromId model) sector.areaId



--| Area


areaFromId : Model -> Int -> Maybe Area
areaFromId model id =
    Dict.get id model.areas
