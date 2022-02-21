module Utilities.EntityFormUtilities exposing (..)

import Data exposing (Area, Ascent, ClimbingRoute, ClimbingRouteKind(..), Sector, Trip, ascentKindToString, climbingRouteKindToString)
import Dict exposing (Dict)
import Message exposing (ItemType(..))
import Model exposing (Criteria, Criterium, EntityForm, FormState(..), Model)
import Set
import Utilities
import Utilities.EntityUtilities exposing (getParent)


closeForm : EntityForm -> EntityForm
closeForm form =
    { form | formState = Hidden }


getFormFromItem : ItemType -> Model -> EntityForm
getFormFromItem item model =
    case item of
        AreaItem ->
            model.areasModel.form

        SectorItem ->
            model.sectorsModel.form

        ClimbingRouteItem ->
            model.climbingRoutesModel.form

        AscentItem ->
            model.ascentsModel.form

        TripItem ->
            model.tripsModel.form


parentIdAccessor : (a -> Maybe Int) -> Maybe a -> String
parentIdAccessor parentId =
    Utilities.maybeAccessor (parentId >> Maybe.map String.fromInt >> Maybe.withDefault "")


getCriteriaFromItem : Int -> ItemType -> Model -> Criteria
getCriteriaFromItem requestId itemType model =
    case itemType of
        ClimbingRouteItem ->
            Dict.get requestId model.climbingRoutes
                |> toClimbingRouteFormCriteria

        AscentItem ->
            Dict.get requestId model.ascents
                |> toAscentFormCriteria

        AreaItem ->
            Dict.get requestId model.areas
                |> toAreaFormCriteria

        SectorItem ->
            Dict.get requestId model.sectors
                |> toSectorFormCriteria

        TripItem ->
            Dict.get requestId model.trips |> toTripFormCriteria


toClimbingRouteFormCriteria : Maybe ClimbingRoute -> Criteria
toClimbingRouteFormCriteria maybeClimbingRoute =
    Dict.fromList
        [ ( "_parentId", { value = parentIdAccessor .sectorId maybeClimbingRoute, label = "_parentId", type_ = Model.Enumeration [] } )
        , ( "name", { value = Utilities.maybeAccessor .name maybeClimbingRoute, label = "name", type_ = Model.String } )
        , ( "grade", { value = Utilities.maybeAccessor .grade maybeClimbingRoute, label = "grade", type_ = Model.String } )
        , ( "comment", { value = Utilities.maybeAccessor (.comment >> Maybe.withDefault "") maybeClimbingRoute, label = "comment", type_ = Model.String } )
        , ( "kind"
          , { value =
                Utilities.maybeAccessor
                    (.kind >> climbingRouteKindToString)
                    maybeClimbingRoute
            , label = "kind"
            , type_ = Model.Enumeration [ "sport", "boulder" ]
            }
          )
        ]


toSectorFormCriteria : Maybe Sector -> Criteria
toSectorFormCriteria maybeSector =
    Dict.fromList
        [ ( "_parentId", { value = parentIdAccessor .areaId maybeSector, label = "_parentId", type_ = Model.Enumeration [] } )
        , ( "name", { value = Utilities.maybeAccessor .name maybeSector, label = "name", type_ = Model.String } )
        ]


toTripFormCriteria : Maybe Trip -> Criteria
toTripFormCriteria maybeTrip =
    Dict.fromList
        []


toAreaFormCriteria : Maybe Area -> Dict String Criterium
toAreaFormCriteria maybeArea =
    Dict.fromList
        [ ( "name", { value = Utilities.maybeAccessor .name maybeArea, label = "name", type_ = Model.String } )
        , ( "country", { value = Utilities.maybeAccessor .country maybeArea, label = "country", type_ = Model.String } )
        ]


toAscentFormCriteria : Maybe Ascent -> Dict String Criterium
toAscentFormCriteria maybeAscent =
    Dict.fromList
        [ ( "_parentId", { value = parentIdAccessor .routeId maybeAscent, label = "_parentId", type_ = Model.Enumeration [] } )
        , ( "comment", { value = Utilities.maybeAccessor (.comment >> Maybe.withDefault "") maybeAscent, label = "comment", type_ = Model.String } )
        , ( "date", { value = Utilities.maybeAccessor (.date >> Maybe.withDefault "") maybeAscent, label = "date", type_ = Model.Date } )
        , ( "kind"
          , { value =
                Utilities.maybeAccessor
                    (.kind >> ascentKindToString)
                    maybeAscent
            , label = "kind"
            , type_ = Model.Enumeration [ "redpoint", "flash", "onsight", "secondgo", "repeat" ]
            }
          )
        ]


getCriteriumValueFromForm : String -> EntityForm -> Maybe String
getCriteriumValueFromForm key form =
    Dict.get key form.criteria |> Maybe.map .value


getNewIdFromFrom : EntityForm -> Dict Int a -> Int
getNewIdFromFrom form collection =
    case form.formState of
        Model.Update id ->
            id

        _ ->
            Utilities.newId collection


getParentFromForm : EntityForm -> Dict Int a -> Maybe a
getParentFromForm form parentCollection =
    form.parentId
        |> Maybe.andThen String.toInt
        |> Maybe.andThen (\id -> Dict.get id parentCollection)


modifiedParentCollection :
    ItemType
    -> Int
    -> Maybe { a | id : Int }
    -> ({ a | id : Int } -> Maybe (Set.Set Int))
    -> (Maybe (Set.Set Int) -> { a | id : Int } -> { a | id : Int })
    -> Model
    -> Dict Int { a | id : Int }
    -> Dict Int { a | id : Int }
modifiedParentCollection childType newId maybeParent childAccessor updateChildIds model parentCollection =
    let
        newChildIds =
            Set.insert
                newId
            <|
                (maybeParent
                    |> Maybe.andThen childAccessor
                    |> Maybe.withDefault Set.empty
                )

        maybeOldParent =
            getParent childType newId model
                |> Maybe.andThen (\i -> Dict.get i parentCollection)

        modifiedCollection =
            maybeParent
                |> Maybe.map (updateChildIds <| Just newChildIds)
                |> Maybe.map (\parent -> Dict.insert parent.id parent parentCollection)
                |> Maybe.withDefault parentCollection

        modifiedOldParent =
            case maybeOldParent of
                Nothing ->
                    modifiedCollection

                Just oldParent ->
                    childAccessor oldParent
                        |> Maybe.map (\children -> Set.remove newId children)
                        |> Maybe.map (\newChildren -> updateChildIds (Just newChildren) oldParent)
                        |> Maybe.map (\parent -> Dict.insert oldParent.id parent modifiedCollection)
                        |> Maybe.withDefault modifiedCollection
    in
    modifiedOldParent


climbingRouteFromForm : Model -> EntityForm -> ( Data.ClimbingRoute, Dict.Dict Int Data.Sector )
climbingRouteFromForm model form =
    let
        newRouteId =
            getNewIdFromFrom form model.climbingRoutes

        maybeSector =
            getParentFromForm form model.sectors

        maybeKind =
            getCriteriumValueFromForm "kind" form

        modifiedSectors =
            modifiedParentCollection ClimbingRouteItem
                newRouteId
                maybeSector
                .routeIds
                (\newIds sector -> { sector | routeIds = newIds })
                model
                model.sectors

        maybeName =
            getCriteriumValueFromForm "name" form

        maybeGrade =
            getCriteriumValueFromForm "grade" form

        maybeComment =
            getCriteriumValueFromForm "comment" form

        maybeAscentIds =
            Dict.get newRouteId model.climbingRoutes |> Maybe.andThen .ascentIds
    in
    ( { id = newRouteId
      , sectorId = Maybe.map .id maybeSector
      , name = Maybe.withDefault "" maybeName
      , grade = Maybe.withDefault "" maybeGrade
      , comment = maybeComment
      , ascentIds = maybeAscentIds
      , kind = Maybe.andThen Data.climbingRouteKindFromString maybeKind |> Maybe.withDefault Sport
      }
    , modifiedSectors
    )


sectorFromForm : Model -> EntityForm -> ( Data.Sector, Dict.Dict Int Data.Area )
sectorFromForm model form =
    let
        newSectorId =
            getNewIdFromFrom form model.sectors

        maybeArea =
            getParentFromForm form model.areas

        modifiedAreas =
            modifiedParentCollection SectorItem
                newSectorId
                maybeArea
                .sectorIds
                (\newIds area -> { area | sectorIds = newIds })
                model
                model.areas

        maybeName =
            getCriteriumValueFromForm "name" form

        maybeRouteIds =
            Dict.get newSectorId model.sectors |> Maybe.andThen .routeIds
    in
    ( { id = newSectorId
      , name = Maybe.withDefault "" maybeName
      , routeIds = maybeRouteIds
      , areaId = Maybe.map .id maybeArea
      }
    , modifiedAreas
    )


ascentFromForm : Model -> EntityForm -> ( Data.Ascent, Dict.Dict Int Data.ClimbingRoute )
ascentFromForm model form =
    let
        newAscentId =
            getNewIdFromFrom form model.ascents

        maybeRoute =
            getParentFromForm form model.climbingRoutes

        modifiedRoutes =
            modifiedParentCollection AscentItem
                newAscentId
                maybeRoute
                .ascentIds
                (\newIds route -> { route | ascentIds = newIds })
                model
                model.climbingRoutes

        maybeComment =
            getCriteriumValueFromForm "comment" form

        maybeDate =
            getCriteriumValueFromForm "date" form

        maybeKind =
            getCriteriumValueFromForm "kind" form
    in
    ( { id = newAscentId
      , comment = maybeComment
      , date = maybeDate
      , routeId = Maybe.map .id maybeRoute
      , kind = Maybe.andThen Data.ascentKindFromString maybeKind |> Maybe.withDefault Data.Redpoint
      }
    , modifiedRoutes
    )


areaFromForm : Model -> EntityForm -> Data.Area
areaFromForm model form =
    let
        newAreaId =
            getNewIdFromFrom form model.areas

        maybeName =
            getCriteriumValueFromForm "name" form

        maybeCountry =
            getCriteriumValueFromForm "country" form

        maybeSectorIds =
            Dict.get newAreaId model.areas |> Maybe.andThen .sectorIds
    in
    { id = newAreaId
    , name = Maybe.withDefault "" maybeName
    , country = Maybe.withDefault "" maybeCountry
    , sectorIds = maybeSectorIds
    }


updateCriterium : String -> String -> Criteria -> Criteria
updateCriterium key value criteria =
    let
        formItem =
            Dict.get key criteria

        updatedFormItem =
            Maybe.map (\item -> { item | value = value }) formItem

        updatedCriteria =
            Maybe.map (\c -> Dict.insert key c criteria) updatedFormItem
    in
    Maybe.withDefault criteria updatedCriteria
