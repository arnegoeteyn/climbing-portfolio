module Utilities.EntityPageUtilities exposing (..)

import Array
import Data exposing (Area, Ascent, ClimbingRoute, ClimbingRouteKind(..), CriteriumValue, ItemPageItem, Sector, ascentKindToString, climbingRouteKindToString)
import Dict exposing (Dict)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import Message exposing (ItemType(..), Route(..))
import Model exposing (Criteria, FormState(..), ItemPageItemForm, ItemPageModel, Model)
import Url.Builder
import Utilities
import Utilities.EntityFormUtilities as ItemFormUtilities
import Utilities.EntityUtilities as EntityUtilities


getItemFromRoute : Route -> Maybe ItemType
getItemFromRoute route =
    case route of
        AreasRoute _ _ ->
            Just AreaItem

        SectorsRoute _ _ ->
            Just SectorItem

        RoutesRoute _ _ ->
            Just ClimbingRouteItem

        AscentsRoute _ _ ->
            Just AscentItem

        _ ->
            Nothing


getModelFromItem : ItemType -> Model -> ItemPageModel
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


getDataFromItem : ItemType -> Model -> Dict Int ItemPageItem
getDataFromItem item model =
    case item of
        ClimbingRouteItem ->
            Dict.map (toClimbingRouteItem model) model.climbingRoutes

        AscentItem ->
            Dict.map (toAscentItem model) model.ascents

        SectorItem ->
            Dict.map (toSectorItem model) model.sectors

        AreaItem ->
            Dict.map toAreaItem model.areas


itemPageTableHeaders : ItemType -> List String
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


getParentName : Model -> ItemType -> Int -> String
getParentName model itemType parentId =
    EntityUtilities.getParent itemType
        |> Maybe.andThen
            (\parentType ->
                Dict.get parentId <|
                    getDataFromItem parentType model
            )
        |> Maybe.map .identifier
        |> Maybe.withDefault ""


toClimbingRouteItem : Model -> Int -> ClimbingRoute -> ItemPageItem
toClimbingRouteItem model _ climbingRoute =
    { cardHeader =
        List.foldr (++) "" <|
            [ climbingRoute.name, " [", climbingRoute.grade, "]" ]
    , identifier = climbingRoute.name
    , cardDescription = climbingRoute.comment
    , tableValues =
        [ ( "name", climbingRoute.name )
        , ( "grade", climbingRoute.grade )
        , ( "kind", climbingRouteKindToString climbingRoute.kind )
        , ( "sector", climbingRoute.sectorId |> Maybe.map (getParentName model ClimbingRouteItem) |> Maybe.withDefault "" )
        ]
    , id = climbingRoute.id
    , parentId = climbingRoute.sectorId
    , childIds = climbingRoute.ascentIds
    }


toSectorItem : Model -> Int -> Sector -> ItemPageItem
toSectorItem model _ sector =
    { cardHeader = sector.name
    , identifier = sector.name
    , id = sector.id
    , tableValues =
        [ ( "name", sector.name )
        , ( "area", sector.areaId |> Maybe.map (getParentName model SectorItem) |> Maybe.withDefault "" )
        ]
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
    , tableValues =
        [ ( "date", Maybe.withDefault "" ascent.date )
        , ( "kind", ascentKindToString ascent.kind )
        , ( "route", parentRouteName )
        ]
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


urlToItem : ItemType -> Int -> String
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


urlToCreateItem : ItemType -> List CriteriumValue -> String
urlToCreateItem item criteria =
    let
        prefix =
            case item of
                Message.AreaItem ->
                    "areas"

                Message.SectorItem ->
                    "sectors"

                Message.ClimbingRouteItem ->
                    "routes"

                Message.AscentItem ->
                    "ascents"
    in
    Url.Builder.absolute [ prefix ] [ Url.Builder.string "criteria" (encode 0 <| Data.encodeCriteriumValueList criteria) ]


paramsFromRoute : ItemPageItemForm -> Route -> ( Maybe Int, Criteria, FormState )
paramsFromRoute form route =
    let
        maybeItem =
            getItemFromRoute route

        ( maybeSelectedId, maybeCriteria, formState ) =
            case maybeItem of
                Just item ->
                    case ( route, item ) of
                        ( Message.RoutesRoute maybeSelected criteria, ClimbingRouteItem ) ->
                            ( maybeSelected
                            , criteria
                            , if criteria == Nothing then
                                Hidden

                              else
                                Create
                            )

                        ( Message.AscentsRoute maybeSelected criteria, AscentItem ) ->
                            ( maybeSelected
                            , criteria
                            , if criteria == Nothing then
                                Hidden

                              else
                                Create
                            )

                        ( Message.AreasRoute maybeSelected criteria, AreaItem ) ->
                            ( maybeSelected
                            , criteria
                            , if criteria == Nothing then
                                Hidden

                              else
                                Create
                            )

                        ( Message.SectorsRoute maybeSelected criteria, SectorItem ) ->
                            ( maybeSelected
                            , criteria
                            , if criteria == Nothing then
                                Hidden

                              else
                                Create
                            )

                        _ ->
                            ( Nothing, Nothing, Hidden )

                Nothing ->
                    ( Nothing, Nothing, Hidden )

        formCriteria =
            form.criteria

        updatedCriteria =
            maybeCriteria
                |> Maybe.map (\c -> decodeString Data.criteriumValueListDecoder c)
                |> Maybe.andThen Result.toMaybe
                |> Maybe.map (List.foldr (\c -> ItemFormUtilities.updateCriterium c.key c.value) formCriteria)
                |> Maybe.withDefault formCriteria
    in
    ( maybeSelectedId, updatedCriteria, formState )


setItemPageModel : ItemPageModel -> Model -> Model
setItemPageModel itemPageModel model =
    case itemPageModel.itemType of
        ClimbingRouteItem ->
            { model | climbingRoutesModel = itemPageModel }

        SectorItem ->
            { model | sectorsModel = itemPageModel }

        AscentItem ->
            { model | ascentsModel = itemPageModel }

        AreaItem ->
            { model | areasModel = itemPageModel }


updateItemPageModelWithParams : ItemPageModel -> ( Maybe Int, Criteria, FormState ) -> ItemPageModel
updateItemPageModelWithParams model ( maybeSelectedId, criteria, formState ) =
    let
        form =
            (\x -> { x | criteria = criteria, formState = formState, parentId = Dict.get "_parentId" criteria |> Maybe.map .value }) model.form
    in
    { model | form = form, selectedItemId = maybeSelectedId }