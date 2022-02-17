module Utilities.EntityPageUtilities exposing (..)

import Data exposing (ClimbingRouteKind(..), CriteriumValue, ascentKindToString, climbingRouteKindToString)
import Dict exposing (Dict)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode)
import Message exposing (ItemType(..), Route(..))
import Model exposing (Criteria, EntityForm, FormState(..), ItemPageModel, Model)
import Url.Builder
import Utilities
import Utilities.EntityFormUtilities as ItemFormUtilities
import Utilities.EntityUtilities as EntityUtilities exposing (getArea, getAscent, getClimbingRoute, getSector)


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


entityPageTableHeaders : ItemType -> List String
entityPageTableHeaders item =
    case item of
        ClimbingRouteItem ->
            [ "name", "grade", "kind", "sector" ]

        AscentItem ->
            [ "date", "kind", "route" ]

        SectorItem ->
            [ "name", "area" ]

        AreaItem ->
            [ "name", "country" ]


selectedItemId : ItemType -> Model -> Maybe Int
selectedItemId type_ model =
    getModelFromItem type_ model |> .selectedItemId


activeFilters : ItemType -> Model -> Dict String String
activeFilters type_ model =
    getModelFromItem type_ model |> .filterValues


tableValues : ItemType -> Int -> Model -> List ( String, String )
tableValues type_ id model =
    Maybe.withDefault [] <|
        case type_ of
            AreaItem ->
                getArea id model
                    |> Maybe.map
                        (\area ->
                            [ ( "name", area.name ), ( "country", area.country ) ]
                        )

            SectorItem ->
                getSector id model
                    |> Maybe.map
                        (\sector ->
                            [ ( "name", sector.name )
                            , ( "area"
                              , sector.areaId
                                    |> Maybe.andThen
                                        (\x ->
                                            EntityUtilities.getArea x model
                                        )
                                    |> Maybe.map .name
                                    |> Maybe.withDefault ""
                              )
                            ]
                        )

            ClimbingRouteItem ->
                getClimbingRoute id model
                    |> Maybe.map
                        (\climbingRoute ->
                            [ ( "name", climbingRoute.name )
                            , ( "grade", climbingRoute.grade )
                            , ( "kind", climbingRouteKindToString climbingRoute.kind )
                            , ( "sector"
                              , climbingRoute.sectorId
                                    |> Maybe.andThen
                                        (\x ->
                                            EntityUtilities.getSector x model
                                        )
                                    |> Maybe.map .name
                                    |> Maybe.withDefault ""
                              )
                            ]
                        )

            AscentItem ->
                getAscent id model
                    |> Maybe.map
                        (\ascent ->
                            [ ( "date", Maybe.withDefault "" ascent.date )
                            , ( "kind", ascentKindToString ascent.kind )
                            , ( "route"
                              , ascent.routeId
                                    |> Maybe.andThen (\x -> Dict.get x model.climbingRoutes)
                                    |> Maybe.map .name
                                    |> Maybe.withDefault ""
                              )
                            ]
                        )


sortedItems : ItemType -> Model -> List Int
sortedItems type_ model =
    let
        extract d f =
            d |> Dict.toList |> List.map Tuple.second |> List.sortBy f |> List.map .id
    in
    case type_ of
        AreaItem ->
            extract model.areas .name

        SectorItem ->
            extract model.sectors .name

        ClimbingRouteItem ->
            extract model.climbingRoutes .grade

        AscentItem ->
            extract model.ascents (.date >> Maybe.withDefault "")


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


paramsFromRoute : EntityForm -> Route -> ( Maybe Int, Criteria, FormState )
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
