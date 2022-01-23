module Utilities.ItemPageUtilities exposing (..)

import Array
import Data exposing (Area, Ascent, ClimbingRoute, ClimbingRouteKind(..), ItemPageItem, Sector, ascentKindToString, climbingRouteKindToString)
import Dict exposing (Dict)
import Init
import Message exposing (Item(..), ItemRelation)
import Model exposing (ItemPageModel, Model)
import Url.Builder
import Utilities


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


getDataFromItem : Item -> Model -> Dict Int ItemPageItem
getDataFromItem item model =
    case item of
        ClimbingRouteItem ->
            Dict.map toClimbingRouteItem model.climbingRoutes

        AscentItem ->
            Dict.map (toAscentItem model) model.ascents

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


itemPageTableHeaders : Item -> List String
itemPageTableHeaders item =
    case item of
        ClimbingRouteItem ->
            [ "name", "grade", "kind" ]

        AscentItem ->
            [ "date", "kind" ]

        SectorItem ->
            [ "name" ]

        AreaItem ->
            [ "name", "country" ]


toClimbingRouteItem : Int -> ClimbingRoute -> ItemPageItem
toClimbingRouteItem _ climbingRoute =
    { cardHeader =
        List.foldr (++) "" <|
            [ climbingRoute.name, " [", climbingRoute.grade, "]" ]
    , identifier = climbingRoute.name
    , cardDescription = climbingRoute.comment
    , tableValues = [ ( "name", climbingRoute.name ), ( "grade", climbingRoute.grade ), ( "kind", climbingRouteKindToString climbingRoute.kind ) ]
    , id = climbingRoute.id
    , parentId = climbingRoute.sectorId
    , childIds = climbingRoute.ascentIds
    }


toSectorItem : Int -> Sector -> ItemPageItem
toSectorItem _ sector =
    { cardHeader = sector.name
    , identifier = sector.name
    , id = sector.id
    , tableValues = [ ( "name", sector.name ) ]
    , cardDescription = Nothing
    , parentId = sector.areaId
    , childIds = sector.routeIds
    }


toAreaItem : Int -> Area -> ItemPageItem
toAreaItem _ area =
    { cardHeader = area.name
    , identifier = area.name
    , id = area.id
    , tableValues = [ ( "name", area.name ), ( "country", area.country ) ]
    , cardDescription = Just area.country
    , parentId = Nothing
    , childIds = area.sectorIds
    }


toAscentItem : Model -> Int -> Ascent -> ItemPageItem
toAscentItem model _ ascent =
    let
        parentRouteName =
            ascent.routeId
                |> Maybe.andThen (\x -> Dict.get x model.climbingRoutes)
                |> Maybe.map .name
                |> Maybe.withDefault ""

        dateAndKind =
            Maybe.withDefault (String.fromInt ascent.id) ascent.date ++ " [" ++ ascentKindToString ascent.kind ++ "]"
    in
    { cardHeader = parentRouteName ++ " ~ " ++ dateAndKind
    , identifier = dateAndKind
    , cardDescription = ascent.comment
    , tableValues = [ ( "date", Maybe.withDefault "" ascent.date ), ( "kind", ascentKindToString ascent.kind ), ( "route", parentRouteName ) ]
    , id = ascent.id
    , parentId = ascent.routeId
    , childIds = Nothing
    }


sortedItems : ItemPageModel -> Model -> List ItemPageItem
sortedItems pageModel model =
    getDataFromItem pageModel.itemType model
        |> Dict.toList
        |> List.map Tuple.second
        |> List.sortBy
            (\a ->
                a.tableValues
                    |> Array.fromList
                    |> Array.get (Maybe.withDefault 0 pageModel.sortOnColumn)
                    |> Maybe.map Tuple.second
                    |> Maybe.withDefault ""
                    |> String.toLower
            )


urlToItem : Item -> Int -> String
urlToItem t id =
    let
        prefix =
            case t of
                Message.AreaItem ->
                    "areas"

                Message.SectorItem ->
                    "sectors"

                Message.ClimbingRouteItem ->
                    "routes"

                Message.AscentItem ->
                    "ascents"
    in
    Url.Builder.absolute [ prefix ] [ Url.Builder.int "selected" id ]
