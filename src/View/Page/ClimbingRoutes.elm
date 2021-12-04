module View.Page.ClimbingRoutes exposing (..)

import Data exposing (ClimbingRoute)
import Dict
import Html.Styled exposing (Html, div, li, option, select, text, ul)
import Html.Styled.Attributes exposing (value)
import Html.Styled.Events exposing (onClick, onInput)
import Message exposing (ClimbingRouteMsg(..), Msg(..))
import Model exposing (Model)
import Utilities exposing (viewInput)
import View.Page.GenericItemPage exposing (viewItemPage)
import View.Widget.GenericItemCard exposing (viewClimbingRouteCard)


viewClimbingRoutes : Model -> Html Msg
viewClimbingRoutes model =
    let
        buttonConfig =
            { addMessage = Message.ClimbingRoute <| ShowNewRouteForm
            , closeMessage = Message.ClimbingRoute <| CloseNewRouteForm
            , saveMessage = SaveRouteRequested
            }

        itemListConfiguration =
            { viewItem = viewClimbingRoute, items = model.climbingRoutes }
    in
    viewItemPage
        { getSelectedItem = model.climbingRoutesModel.selectedRoute
        , viewItemCard = viewClimbingRouteCard
        , viewForm = viewRouteForm
        , itemListConfiguration = itemListConfiguration
        , buttonConfiguration = buttonConfig
        }
        model


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
viewClimbingRoute route _ =
    li
        [ onClick <| Message.ClimbingRoute <| Message.ClimbingRouteSelected route ]
        [ text <| route.name ++ " " ++ route.grade ]
