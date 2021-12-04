module View.Page.GenericItemPage exposing (..)

import Dict exposing (Dict(..))
import Html.Styled exposing (Html, button, div, text, ul)
import Html.Styled.Events exposing (onClick)
import Message exposing (ClimbingRouteMsg(..), DataUpdateMsg(..), Item(..), Msg(..))
import Model exposing (Model)
import Svg.Styled.Attributes exposing (css)
import Tailwind.Utilities as Tw


type alias ItemPageConfiguration a =
    { viewForm : Model -> Html Msg
    , itemListConfiguration : ItemListConfiguration a
    , viewItemCard : Maybe a -> Model -> Html Msg
    , getSelectedItem : Maybe a
    , buttonConfiguration : AddItemButtonConfiguration
    }


viewItemPage : ItemPageConfiguration a -> Model -> Html Msg
viewItemPage config model =
    div []
        [ viewAddItemButton config.buttonConfiguration model
        , config.viewForm model
        , div
            [ css [ Tw.grid, Tw.grid_cols_2 ] ]
            [ viewItemList config.itemListConfiguration model
            , div [ css [ Tw.flex, Tw.justify_center ] ] [ (config.viewItemCard <| config.getSelectedItem) model ]
            ]
        ]


type alias AddItemButtonConfiguration =
    { addMessage : Msg
    , closeMessage : Msg
    , saveMessage : Msg
    }


viewAddItemButton : AddItemButtonConfiguration -> Model -> Html Msg
viewAddItemButton config model =
    let
        addButton =
            button [ onClick config.addMessage ] [ text "New" ]

        closeButton =
            button [ onClick config.closeMessage ] [ text "Close" ]

        saveButton =
            button [ onClick config.saveMessage ] [ text "Save" ]
    in
    case model.climbingRoutesModel.form of
        Nothing ->
            div [] [ addButton ]

        Just _ ->
            div []
                [ saveButton
                , closeButton
                ]


type alias ItemListConfiguration a =
    { viewItem : a -> Model -> Html Msg
    , items : Dict Int a
    }


viewItemList : ItemListConfiguration a -> Model -> Html Msg
viewItemList config model =
    ul [] <|
        List.map (\a -> config.viewItem a model) <|
            Dict.values config.items
