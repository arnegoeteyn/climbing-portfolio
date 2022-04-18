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
import Tailwind.Utilities as Tw
import Utilities
import Utilities.Entity2Utilities as EntityUtilities
import View.Components.Div as Div
import View.Components.Table as Table


view : Model -> Html Msg
view model =
    H.div []
        [ viewFilters model
        , H.div [ A.css [] ] <|
            List.map
                (\route ->
                    H.div [ A.css [ Tw.border, Tw.border_solid, Tw.py_4 ] ]
                        [ viewRouteRow model route, viewRouteDetail model route ]
                )
                (sortedAndFilteredRoutes model)

        -- ]
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
    let
        hasMedia =
            (not << List.isEmpty) route.media

        addMediaInput =
            Utilities.viewInput "text" "media" model.overviewModel.mediaInput (Message.Overview << Message.OnMediaInput)

        addMediaButton =
            H.button [ E.onClick <| Message.AddMediaToRoute route ] [ H.text "add" ]
    in
    if isSelected model route then
        H.div [ A.css [ Tw.grid, Tw.gap_4, Tw.grid_cols_2 ] ]
            [ H.div [ A.css [ Tw.p_2, Tw.col_auto, Tw.grid, Tw.justify_items_center ] ]
                [ viewRouteInfo model route
                , viewAscentsList model route
                ]

            -- ]
            , H.div [ A.css [] ]
                [ H.img
                    (A.src
                        "https://www.barcelona-tourist-guide.com/images/ext/attractions/montserrat/L550/montserrat-barcelona-29.jpg"
                        :: [ A.css [ Tw.col_auto, Tw.mx_auto ] ]
                    )
                    []
                , H.div [ Div.importantProperties ] [ H.text <| Utilities.stringFromList [ String.fromInt <| List.length route.media, " media:" ] ]
                , if hasMedia then
                    H.ul [] <| List.map (\m -> H.li [] [ H.a [ A.css [ Tw.break_words ], A.href m, A.target "_blank" ] [ H.text m ] ]) route.media

                  else
                    H.text ""
                , H.div []
                    [ addMediaInput
                    , addMediaButton
                    ]
                ]
            ]

    else
        H.text ""


viewRouteInfo : Model -> ClimbingRoute -> Html Msg
viewRouteInfo model climbingRoute =
    H.div [ A.css [] ]
        [ H.text <| Maybe.withDefault "" climbingRoute.comment
        ]


viewAscentsList : Model -> ClimbingRoute -> Html Msg
viewAscentsList model route =
    let
        ascents =
            EntityUtilities.getAscents model route
    in
    H.div [ A.css [] ]
        (H.div [ Div.importantProperties ] [ H.text (Utilities.stringFromList [ String.fromInt <| List.length ascents, " ascents:" ]) ]
            :: List.map
                (\ascent -> H.div [] [ H.text <| Maybe.withDefault "" <| Maybe.map Date.toIsoString <| ascent.date ])
                ascents
        )


isSelected : Model -> ClimbingRoute -> Bool
isSelected model route =
    model.overviewModel.selectedClimbingRoute == Just route


viewRouteRow : Model -> ClimbingRoute -> Html Msg
viewRouteRow model route =
    H.div
        ([ E.onClick <|
            (Message.Overview << Message.OnClimbingRouteClicked) (Just route)
         , A.css [ Tw.flex ]
         ]
            ++ Utilities.filterAndReplaceList
                [ ( Table.selectedRowProperties, isSelected model route, Nothing )
                ]
        )
        [ H.div [ A.css [ Tw.w_1over6 ] ] [ H.div [ Div.largeProperties ] [ H.text route.grade ] ]
        , H.div [ A.css [ Tw.w_2over6 ] ] [ viewRouteNameCell model route ]
        , H.div [ A.css [ Tw.w_2over6 ] ] [ H.text (Data.climbingRouteKindToString route.kind) ]
        , H.div [ A.css [ Tw.w_1over6 ] ] [ (H.text << String.fromInt << Set.size) route.ascentIds ]
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
