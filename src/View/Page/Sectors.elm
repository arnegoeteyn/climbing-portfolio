module View.Page.Sectors exposing (..)

import Data exposing (Sector)
import Html.Styled exposing (Html, button, div, li, option, select, text, ul)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onClick, onInput)
import Message exposing (ClimbingRouteMsg(..), Msg(..), SectorMsg(..))
import Model exposing (Model)
import Utilities exposing (viewInput)
import View.Page.GenericItemPage exposing (viewItemPage)
import View.Widget.GenericItemCard exposing (viewClimbingRouteCard)


viewSectors : Model -> Html Msg
viewSectors model =
    let
        buttonConfig =
            { addMessage = Message.Sector <| ShowNewSectorForm
            , closeMessage = Message.Sector <| CloseNewSectorForm
            , saveMessage = Dummy
            }

        itemListConfiguration =
            { viewItem = viewSector, items = model.sectors }
    in
    viewItemPage
        { getSelectedItem = model.sectorsModel.selectedSector
        , viewItemCard = \m -> \a -> text "Abc"
        , viewForm = \a -> text "Form"
        , itemListConfiguration = itemListConfiguration
        , buttonConfiguration = buttonConfig
        }
        model



-- viewRouteForm : Model -> Html Msg
-- viewRouteForm model =
--     case model.climbingRoutesModel.form of
--         Nothing ->
--             text ""
--         Just justForm ->
--             div []
--                 [ viewInput "text" "Name" justForm.name (Message.ClimbingRoute << Message.FormName)
--                 , viewInput "text" "Grade" justForm.grade (Message.ClimbingRoute << FormGrade)
--                 , select [ onInput (Message.ClimbingRoute << FormSector) ] <|
--                     option [ value "" ] [ text "" ]
--                         :: (Dict.values model.sectors |> List.map (\sector -> option [ value <| String.fromInt sector.id ] [ text sector.name ]))
--                 ]


viewSector : Sector -> Model -> Html Msg
viewSector sector _ =
    li
        [ onClick <| Message.Sector <| Message.SectorSelected sector ]
        [ text <| sector.name ]
