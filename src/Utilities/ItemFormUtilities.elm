module Utilities.ItemFormUtilities exposing (..)

import Data exposing (Area, Ascent, ClimbingRoute, ClimbingRouteKind(..), Sector, ascentKindToString, climbingRouteKindToString)
import Dict exposing (Dict)
import Json.Decode
import Message exposing (Item(..))
import Model exposing (Criterium, ItemPageItemForm, Model)
import Set exposing (Set)
import Utilities


parentIdAccessor : (a -> Maybe Int) -> Maybe a -> String
parentIdAccessor parentId =
    Utilities.maybeAccessor (parentId >> Maybe.map String.fromInt >> Maybe.withDefault "")


getCriteriaFromItem : Int -> Item -> Model -> Dict String Criterium
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


toClimbingRouteFormCriteria : Maybe ClimbingRoute -> Dict String Criterium
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


toSectorFormCriteria : Maybe Sector -> Dict String Criterium
toSectorFormCriteria maybeSector =
    Dict.fromList
        [ ( "_parentId", { value = parentIdAccessor .areaId maybeSector, label = "_parentId", type_ = Model.Enumeration [] } )
        , ( "name", { value = Utilities.maybeAccessor .name maybeSector, label = "name", type_ = Model.String } )
        ]


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


getCriteriumValueFromForm : String -> ItemPageItemForm -> Maybe String
getCriteriumValueFromForm key form =
    Dict.get key form.criteria |> Maybe.map .value


getNewIdFromFrom : ItemPageItemForm -> Dict Int a -> Int
getNewIdFromFrom form collection =
    case form.formState of
        Model.Update id ->
            id

        _ ->
            Utilities.newId collection


getParentFromForm : ItemPageItemForm -> Dict Int a -> Maybe a
getParentFromForm form parentCollection =
    form.parentId
        |> Maybe.andThen String.toInt
        |> Maybe.andThen (\id -> Dict.get id parentCollection)


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
                |> Maybe.map (\parent -> Dict.insert parent.id parent parentCollection)
                |> Maybe.withDefault parentCollection
    in
    modifiedCollection


climbingRouteFromForm : Model -> ItemPageItemForm -> ( Data.ClimbingRoute, Dict.Dict Int Data.Sector )
climbingRouteFromForm model form =
    let
        newRouteId =
            getNewIdFromFrom form model.climbingRoutes

        maybeSector =
            getParentFromForm form model.sectors

        maybeKind =
            getCriteriumValueFromForm "kind" form

        modifiedSectors =
            modifiedParentCollection newRouteId
                maybeSector
                .routeIds
                (\newIds sector -> { sector | routeIds = newIds })
                model.sectors

        maybeName =
            getCriteriumValueFromForm "name" form

        maybeGrade =
            getCriteriumValueFromForm "grade" form

        maybeComment =
            getCriteriumValueFromForm "comment" form
    in
    ( { id = newRouteId
      , sectorId = Maybe.map .id maybeSector
      , name = Maybe.withDefault "" maybeName
      , grade = Maybe.withDefault "" maybeGrade
      , comment = maybeComment
      , ascentIds = Just Set.empty
      , kind = Maybe.andThen (Result.toMaybe << Json.Decode.decodeString Data.climbingRouteKindDecoder) maybeKind |> Maybe.withDefault Sport
      }
    , modifiedSectors
    )


sectorFromForm : Model -> ItemPageItemForm -> ( Data.Sector, Dict.Dict Int Data.Area )
sectorFromForm model form =
    let
        newSectorId =
            getNewIdFromFrom form model.sectors

        maybeArea =
            getParentFromForm form model.areas

        modifiedAreas =
            modifiedParentCollection newSectorId
                maybeArea
                .sectorIds
                (\newIds area -> { area | sectorIds = newIds })
                model.areas

        maybeName =
            getCriteriumValueFromForm "name" form
    in
    ( { id = newSectorId
      , name = Maybe.withDefault "" maybeName
      , routeIds = Nothing
      , areaId = Maybe.map .id maybeArea
      }
    , modifiedAreas
    )


ascentFromForm : Model -> ItemPageItemForm -> ( Data.Ascent, Dict.Dict Int Data.ClimbingRoute )
ascentFromForm model form =
    let
        newAscentId =
            getNewIdFromFrom form model.ascents

        maybeRoute =
            getParentFromForm form model.climbingRoutes

        modifiedRoutes =
            modifiedParentCollection newAscentId
                maybeRoute
                .ascentIds
                (\newIds route -> { route | ascentIds = newIds })
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
      , kind = Maybe.andThen (Result.toMaybe << Json.Decode.decodeString Data.ascentKindDecoder) maybeKind |> Maybe.withDefault Data.Redpoint
      }
    , modifiedRoutes
    )


areaFromForm : Model -> ItemPageItemForm -> Data.Area
areaFromForm model form =
    let
        newAreaId =
            getNewIdFromFrom form model.areas

        maybeName =
            getCriteriumValueFromForm "name" form

        maybeCountry =
            getCriteriumValueFromForm "country" form
    in
    { id = newAreaId
    , name = Maybe.withDefault "" maybeName
    , country = Maybe.withDefault "" maybeCountry
    , sectorIds = Nothing
    }
