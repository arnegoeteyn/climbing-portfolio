module Utilities.ItemPageUtilities exposing (..)

import Data exposing (Area, Ascent, ClimbingRoute, ItemPageItem, Sector)
import Dict exposing (Dict)
import Init exposing (areaForm, ascentForm, climbingRouteForm, sectorForm)
import Message exposing (Item(..), ItemRelation)
import Model exposing (Criterium, ItemPageItemForm, ItemPageModel, Model)
import Set
import Utilities exposing (newId)


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
    , cardDescription = climbingRoute.description
    , id = climbingRoute.id
    , parentId = climbingRoute.sectorId
    , childIds = climbingRoute.ascentIds
    }


toClimbingRouteFormCriteria : ClimbingRoute -> Dict String Criterium
toClimbingRouteFormCriteria route =
    Dict.fromList
        [ ( "_parentId", { value = route.sectorId |> Maybe.map String.fromInt |> Maybe.withDefault "", label = "_parentId", type_ = Model.Enumeration } )
        , ( "name", { value = route.name, label = "name", type_ = Model.String } )
        , ( "grade", { value = route.grade, label = "grade", type_ = Model.String } )
        , ( "description", { value = Maybe.withDefault "" route.description, label = "description", type_ = Model.String } )
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
    Dict.fromList
        [ ( "_parentId", { value = sector.areaId |> Maybe.map String.fromInt |> Maybe.withDefault "", label = "_parentId", type_ = Model.Enumeration } )
        , ( "name", { value = sector.name, label = "name", type_ = Model.String } )
        ]


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
    Dict.fromList
        [ ( "name", { value = area.name, label = "name", type_ = Model.Enumeration } )
        , ( "country", { value = area.country, label = "country", type_ = Model.String } )
        ]


toAscentItem : Int -> Ascent -> ItemPageItem
toAscentItem _ ascent =
    { cardHeader = Maybe.withDefault (String.fromInt ascent.id) ascent.date
    , identifier = Maybe.withDefault (String.fromInt ascent.id) ascent.date
    , cardDescription = ascent.description
    , id = ascent.id
    , parentId = ascent.routeId
    , childIds = Nothing
    }


toAscentFormCriteria : Ascent -> Dict String Criterium
toAscentFormCriteria ascent =
    Dict.fromList
        [ ( "_parentId", { value = ascent.routeId |> Maybe.map String.fromInt |> Maybe.withDefault "", label = "_parentId", type_ = Model.Enumeration } )
        , ( "description", { value = Maybe.withDefault "" ascent.description, label = "description", type_ = Model.String } )
        , ( "date", { value = Maybe.withDefault "" ascent.date, label = "date", type_ = Model.Date } )
        ]
