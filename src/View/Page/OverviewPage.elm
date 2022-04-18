module View.Page.OverviewPage exposing (..)

import Data exposing (ClimbingRoute, Sector)
import Date
import Dict
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Message exposing (Msg)
import Model exposing (Model)
import Select
import Set
import Utilities
import Utilities.Entity2Utilities as EntityUtilities
import View.Components.Div as Div
import View.Components.Table as Table


view : Model -> Html Msg
view model =
    H.div []
        [ viewFilters model
        , H.table
            [ Table.tableProperties ]
          <|
            [ H.tbody [ Table.tableBodyProperties ]
                (List.concatMap
                    (\route -> [ viewRouteRow model route, viewRouteDetail model route ])
                    (sortedAndFilteredRoutes model)
                )
            ]
        ]


sectorSelectConfig : Select.Config Message.Msg Sector
sectorSelectConfig =
    let
        r : Select.RequiredConfig Message.Msg Sector
        r =
            { filter = \x y -> filterSectorsByName x y |> Utilities.listToMaybe
            , toLabel = .name
            , onSelect = Message.Overview << Message.OnSelect
            , toMsg = Message.Overview << Message.SelectMsg
            }
    in
    Select.newConfig r
        |> Select.withMultiSelection True
        |> Select.withOnRemoveItem (Message.Overview << Message.OnRemove)


viewFilters : Model -> Html Msg
viewFilters model =
    H.div []
        [ Utilities.viewInput "text"
            "route"
            model.overviewModel.routeFilter
            (\value -> Message.Overview (Message.RouteFilter value))
        , H.fromUnstyled <|
            Select.view
                sectorSelectConfig
                model.overviewModel.selectState
                (Dict.toList model.sectors |> List.map Tuple.second)
                model.overviewModel.selected
        ]


viewRouteDetail : Model -> ClimbingRoute -> Html Msg
viewRouteDetail model route =
    if isSelected model route then
        H.tr [] [ viewRouteInfo model route, viewAscentsList model route ]

    else
        H.text ""


viewRouteInfo : Model -> ClimbingRoute -> Html Msg
viewRouteInfo model climbingRoute =
    H.div []
        [ H.text <| Maybe.withDefault "" climbingRoute.comment
        ]


viewAscentsList : Model -> ClimbingRoute -> Html Msg
viewAscentsList model route =
    let
        ascents =
            EntityUtilities.getAscents model route
    in
    H.div [] <|
        List.map
            (\ascent -> H.div [] [ H.text <| Maybe.withDefault "" <| Maybe.map Date.toIsoString <| ascent.date ])
            ascents


isSelected : Model -> ClimbingRoute -> Bool
isSelected model route =
    model.overviewModel.selectedClimbingRoute == Just route


viewRouteRow : Model -> ClimbingRoute -> Html Msg
viewRouteRow model route =
    H.tr
        ((E.onClick <|
            (Message.Overview << Message.OnClimbingRouteClicked) (Just route)
         )
            :: Utilities.filterAndReplaceList
                [ ( Table.selectedRowProperties, isSelected model route, Nothing )
                ]
        )
        [ H.td [] [ H.div [ Div.largeProperties ] [ H.text route.grade ] ]
        , H.td [] [ viewRouteNameCell model route ]
        , H.td [] [ H.text (Data.climbingRouteKindToString route.kind) ]
        , H.td [] [ (H.text << String.fromInt << Set.size) route.ascentIds ]
        ]


viewRouteNameCell : Model -> ClimbingRoute -> Html Msg
viewRouteNameCell model route =
    let
        sector =
            EntityUtilities.getSector model route
    in
    H.div []
        [ H.div [ Div.importantProperties ] [ H.text route.name ]
        , H.div [] [ H.text (Maybe.withDefault "N/A" <| Maybe.map .name sector) ]
        ]



--| Utilities


sortedAndFilteredRoutes : Model -> List ClimbingRoute
sortedAndFilteredRoutes model =
    let
        routes =
            Dict.toList model.climbingRoutes |> List.map Tuple.second
    in
    (filterRoutes model >> sortRoutes model) routes


sortRoutes : Model -> List ClimbingRoute -> List ClimbingRoute
sortRoutes model =
    EntityUtilities.sortByGrade


filterRoutes : Model -> List ClimbingRoute -> List ClimbingRoute
filterRoutes model routes =
    let
        filter =
            filterRoutesByName model.overviewModel.routeFilter >> filterRoutesBySectors model.overviewModel.selected
    in
    filter routes


filterRoutesByName : String -> List ClimbingRoute -> List ClimbingRoute
filterRoutesByName filter =
    List.filter (\route -> String.contains (String.toLower filter) (String.toLower route.name))


matchRouteBySectors : List Sector -> ClimbingRoute -> Bool
matchRouteBySectors sectors route =
    Maybe.map (\x -> List.isEmpty sectors || List.member x (List.map .id sectors)) route.sectorId |> Maybe.withDefault False


filterRoutesBySectors : List Sector -> List ClimbingRoute -> List ClimbingRoute
filterRoutesBySectors sectors =
    List.filter (matchRouteBySectors sectors)


matchSectorByName : String -> Sector -> Bool
matchSectorByName filter sector =
    String.contains (String.toLower filter) (String.toLower sector.name)


filterSectorsByName : String -> List Sector -> List Sector
filterSectorsByName filter sectors =
    List.filter (matchSectorByName filter) sectors
