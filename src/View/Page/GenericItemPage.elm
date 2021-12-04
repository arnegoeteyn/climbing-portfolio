module View.Page.GenericItemPage exposing (..)

import Data exposing (ClimbingRoute, ItemPageItem, Sector)
import Dict exposing (Dict(..))
import Html.Styled exposing (Html, button, div, li, text, ul)
import Html.Styled.Events exposing (onClick)
import Message exposing (ClimbingRouteMsg(..), Item, ItemPageMsg(..), Msg(..))
import Model exposing (ItemPageModel, Model)
import Svg.Styled.Attributes exposing (css)
import Tailwind.Utilities as Tw



-- type alias ItemPageConfiguration a =
--     { viewForm : Model -> Html Msg
--     , itemListConfiguration : ItemListConfiguration a
--     , viewItemCard : Maybe a -> Model -> Html Msg
--     , getSelectedItem : Maybe a
--     , buttonConfiguration : AddItemButtonConfiguration
--     }
-- viewItemPage : ItemPageConfiguration a -> Model -> Html Msg
-- viewItemPage config model =
--     div []
--         [ viewAddItemButton config.buttonConfiguration model
--         , config.viewForm model
--         , div
--             [ css [ Tw.grid, Tw.grid_cols_2 ] ]
--             [ viewItemList config.itemListConfiguration model
--             , div [ css [ Tw.flex, Tw.justify_center ] ] [ (config.viewItemCard <| config.getSelectedItem) model ]
--             ]
--         ]


viewItemPage : Dict Int ItemPageItem -> ItemPageModel -> Model -> Html Msg
viewItemPage items itemPageModel model =
    div []
        [ viewAddItemButton itemPageModel
            model

        -- , config.viewForm model
        , div
            [ css [ Tw.grid, Tw.grid_cols_2 ] ]
            [ viewItemList items itemPageModel model
            , div [ css [ Tw.flex, Tw.justify_center ] ] [ text <| (Maybe.map String.fromInt itemPageModel.selectedItemId |> Maybe.withDefault "") ]

            -- , div [ css [ Tw.flex, Tw.justify_center ] ] [ (config.viewItemCard <| config.getSelectedItem) model ]
            ]
        ]


type alias AddItemButtonConfiguration =
    { addMessage : Msg
    , closeMessage : Msg
    , saveMessage : Msg
    }


viewAddItemButton : ItemPageModel -> Model -> Html Msg
viewAddItemButton itemPageModel model =
    let
        addButton =
            button [ onClick (ItemPage OpenForm itemPageModel.itemType) ] [ text "New" ]

        closeButton =
            button [ onClick (ItemPage CloseForm itemPageModel.itemType) ] [ text "Close" ]

        saveButton =
            button [] [ text "Save" ]
    in
    case itemPageModel.form of
        Nothing ->
            div [] [ addButton ]

        Just _ ->
            div []
                [ saveButton
                , closeButton
                ]


viewItemList : Dict Int ItemPageItem -> ItemPageModel -> Model -> Html Msg
viewItemList items itemPageModel model =
    ul [] <|
        List.map (\item -> li [ onClick <| ItemPage (SelectItem item.id) itemPageModel.itemType ] [ text item.identifier ]) <|
            Dict.values items



-- getItems : Item -> Model -> List Item
-- getItems item model =
--     let
--         a : Dict Int a
--         a =
--             case item of
--                 ClimbingRouteItem ->
--                     model.climbingRoutes
--                 AscentItem ->
--                     model.ascents
--                 SectorItem ->
--                     model.sectors
--     in
--     Dict.values a
-- type alias ItemListConfiguration a =
--     { viewItem : a -> Model -> Html Msg
--     , items : Dict Int a
--     }
-- viewItemList : ItemListConfiguration a -> Model -> Html Msg
-- viewItemList config model =
--     ul [] <|
--         List.map (\a -> config.viewItem a model) <|
--             Dict.values config.items
-- type alias ItemPageModel =
--     { items : Dict Int ItemPageItem
--     , itemType : Item
--     , showForm : Bool
--     }
-- type alias ItemPageItem =
--     { cardHeader : String
--     , identifier : String
--     }
