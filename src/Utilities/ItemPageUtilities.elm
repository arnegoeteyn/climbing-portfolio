module Utilities.ItemPageUtilities exposing (..)

import Data exposing (Area, Ascent, ClimbingRoute, ClimbingRouteKind(..), ItemPageItem, Sector, ascentKindToString, climbingRouteKindToString)
import Dict exposing (Dict)
import Init
import Message exposing (Item(..), ItemRelation)
import Model exposing (Criterium, ItemPageItemForm, ItemPageModel, Model)
import Set
import Utilities exposing (newId)
import Utilities.ItemFormUtilities as ItemFormUtilities


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


getCriteriaFromItem : Int -> Item -> Model -> Dict String Criterium
getCriteriaFromItem requestId itemType model =
    case itemType of
        ClimbingRouteItem ->
            Dict.get requestId model.climbingRoutes
                |> ItemFormUtilities.toClimbingRouteFormCriteria

        AscentItem ->
            Dict.get requestId model.ascents
                |> ItemFormUtilities.toAscentFormCriteria

        AreaItem ->
            Dict.get requestId model.areas
                |> ItemFormUtilities.toAreaFormCriteria

        SectorItem ->
            Dict.get requestId model.sectors
                |> ItemFormUtilities.toSectorFormCriteria


getParentFromForm : ItemPageItemForm -> Dict Int a -> Maybe a
getParentFromForm form parentCollection =
    form.parentId
        |> Maybe.andThen String.toInt
        |> Maybe.andThen (\id -> Dict.get id parentCollection)


getNewIdFromFrom : ItemPageItemForm -> Dict Int a -> Int
getNewIdFromFrom form collection =
    case form.formState of
        Model.Update id ->
            id

        _ ->
            newId collection


modifiedParentCollection :
    Int
    -> Maybe { a | id : Int }
    -> ({ a | id : Int } -> Maybe (Set.Set Int))
    -> (Maybe (Set.Set Int) -> { a | id : Int } -> { a | id : Int })
    -> Dict Int { a | id : Int }
    -> Dict Int { a | id : Int }
modifiedParentCollection newId maybeParent childAccessor updateChildIds parentCollection =
    let
        newChildIds =
            Set.insert
                newId
            <|
                (maybeParent
                    |> Maybe.andThen childAccessor
                    |> Maybe.withDefault Set.empty
                )

        modifiedCollection =
            maybeParent
                |> Maybe.map (updateChildIds <| Just newChildIds)
                -- (\parent -> { parent | childAccessor = Just newChildIds })
                |> Maybe.map (\parent -> Dict.insert parent.id parent parentCollection)
                |> Maybe.withDefault parentCollection
    in
    modifiedCollection


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
