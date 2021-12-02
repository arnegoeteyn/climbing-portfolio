module View.Page.ClimbingRoutes exposing (..)

import Data exposing (ClimbingRoute)
import Dict
import Html.Styled exposing (Html, button, div, li, text, ul)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Message exposing (ClimbingRouteMsg(..), Msg(..))
import Model exposing (Model)
import Tailwind.Utilities as Tw
import Utilities exposing (viewInput)
import View.Widget.ClimbingRouteCard exposing (viewClimbingRouteCard)


viewClimbingRoutes : Model -> Html Msg
viewClimbingRoutes model =
    let
        addRouteButton =
            case model.climbingRoutesModel.form of
                Nothing ->
                    button [ onClick (Message.ClimbingRoute <| ShowNewRouteForm) ] [ text "add route" ]

                Just form ->
                    div []
                        [ button [ onClick (Message.ClimbingRoute CloseNewRouteForm) ] [ text "close form" ]
                        , button [ onClick Message.SaveRouteRequested ] [ text "save form" ]
                        ]
    in
    div []
        [ addRouteButton
        , viewRouteForm model
        , div
            [ css [ Tw.grid, Tw.grid_cols_2 ] ]
            [ ul [] <|
                List.map (\r -> viewClimbingRoute r model) <|
                    Dict.values model.climbingRoutes
            , div [ css [ Tw.flex, Tw.justify_center ] ] [ viewClimbingRouteCard model.climbingRoutesModel.selectedRoute model ]
            ]
        ]


viewRouteForm : Model -> Html Msg
viewRouteForm model =
    case model.climbingRoutesModel.form of
        Nothing ->
            text ""

        Just justForm ->
            div []
                [ viewInput "text" "Name" justForm.name (Message.ClimbingRoute << Message.FormName)
                , viewInput "text" "Grade" justForm.grade (Message.ClimbingRoute << FormGrade)
                ]


viewClimbingRoute : ClimbingRoute -> Model -> Html Msg
viewClimbingRoute route model =
    li
        [ onClick <| Message.ClimbingRoute <| Message.ClimbingRouteSelected route ]
        [ text <| route.name ++ " " ++ route.grade ]
