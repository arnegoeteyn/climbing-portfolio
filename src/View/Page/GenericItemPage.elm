module View.Page.GenericItemPage exposing (..)

import Data exposing (ItemPageItem)
import Dict exposing (Dict(..))
import Html.Styled exposing (Html, button, div, li, option, select, text, ul)
import Html.Styled.Attributes exposing (value)
import Html.Styled.Events exposing (onClick, onInput)
import Message exposing (ClimbingRouteMsg(..), CriteriumUpdate(..), ItemPageMsg(..), Msg(..))
import Model exposing (ItemPageModel, Model)
import Svg.Styled.Attributes exposing (css)
import Tailwind.Utilities as Tw
import Utilities exposing (viewInput)
import Utilities.ItemPageUtilities as ItemPageUtilities exposing (getDataFromItem)
import View.Widget.GenericItemCard as GenericItemCard



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


viewItemForm : ItemPageModel -> Model -> Html Msg
viewItemForm itemPageModel model =
    let
        viewCriterium criteria key =
            let
                maybeCriterium =
                    Dict.get key criteria
            in
            case maybeCriterium of
                Nothing ->
                    div [] []

                Just criterium ->
                    viewInput "text" criterium.label criterium.value (\value -> ItemPage itemPageModel.itemType (FormUpdateMessage <| UpdateKey key value))

        maybeParentCriterium =
            ItemPageUtilities.getRelationFromItem itemPageModel.itemType
                |> .parent
                |> Maybe.map
                    (\parentItem ->
                        select [ onInput (\value -> ItemPage itemPageModel.itemType (FormUpdateMessage <| UpdateParent value)) ] <|
                            option [ value "" ] [ text "" ]
                                :: (getDataFromItem parentItem model
                                        |> Dict.values
                                        |> List.map (\item -> option [ value <| String.fromInt item.id ] [ text item.identifier ])
                                   )
                    )
    in
    div [] <|
        case itemPageModel.form of
            Nothing ->
                [ text "" ]

            Just justForm ->
                [ div [] <| List.map (viewCriterium justForm.criteria) justForm.order
                , div [] [ Maybe.withDefault (text "") maybeParentCriterium ]
                ]



-- div [] <| Dict.foldl viewCriterium [] justForm.criteria.criteria
-- [ viewInput "text" "Name" justForm.name (Message.ClimbingRoute << Message.FormName)
-- , viewInput "text" "Grade" justForm.grade (Message.ClimbingRoute << FormGrade)
-- , select [ onInput (Message.ClimbingRoute << FormSector) ] <|
--     option [ value "" ] [ text "" ]
--         :: (Dict.values model.sectors |> List.map (\sector -> option [ value <| String.fromInt sector.id ] [ text sector.name ]))
-- ]


viewItemPage : Dict Int ItemPageItem -> ItemPageModel -> Model -> Html Msg
viewItemPage items itemPageModel model =
    div []
        [ viewAddItemButton itemPageModel
            model
        , viewItemForm itemPageModel model
        , div
            [ css [ Tw.grid, Tw.grid_cols_2 ] ]
            [ viewItemList items itemPageModel model
            , div [ css [ Tw.flex, Tw.justify_center ] ] [ GenericItemCard.view itemPageModel model ]
            ]
        ]


viewAddItemButton : ItemPageModel -> Model -> Html Msg
viewAddItemButton itemPageModel model =
    let
        addButton =
            button [ onClick (ItemPage itemPageModel.itemType OpenForm) ] [ text "New" ]

        closeButton =
            button [ onClick (ItemPage itemPageModel.itemType CloseForm) ] [ text "Close" ]

        saveButton =
            button [ onClick (SaveItemRequested itemPageModel.itemType) ] [ text "Save" ]
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
        List.map (\item -> li [ onClick <| ItemPage itemPageModel.itemType (SelectItem item.id) ] [ text item.identifier ]) <|
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
