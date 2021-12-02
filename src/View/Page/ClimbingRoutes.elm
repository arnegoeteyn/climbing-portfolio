module View.Page.ClimbingRoutes exposing (..)

import Data exposing (ClimbingRoute)
import Dict
import Html.Styled exposing (Html, button, div, li, option, select, text, ul)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onClick, onInput)
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

                Just _ ->
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
                , select [ onInput (Message.ClimbingRoute << FormSector) ] <|
                    option [ value "" ] [ text "" ]
                        :: (Dict.values model.sectors |> List.map (\sector -> option [ value <| String.fromInt sector.id ] [ text sector.name ]))
                ]


viewClimbingRoute : ClimbingRoute -> Model -> Html Msg
viewClimbingRoute route model =
    li
        [ onClick <| Message.ClimbingRoute <| Message.ClimbingRouteSelected route ]
        [ text <| route.name ++ " " ++ route.grade ]
