module View.Page.GenericItemPage exposing (..)

import Data exposing (ItemPageItem)
import Dict exposing (Dict(..))
import Html.Styled exposing (Html, button, div, li, option, select, text, ul)
import Html.Styled.Attributes exposing (value)
import Html.Styled.Events exposing (onClick, onInput)
import Message exposing (ClimbingRouteMsg(..), CriteriumUpdate(..), ItemPageMsg(..), Msg(..))
import Model exposing (FormState(..), ItemPageModel, Model)
import Svg.Styled.Attributes exposing (css)
import Tailwind.Utilities as Tw
import Utilities exposing (viewInput)
import Utilities.ItemPageUtilities as ItemPageUtilities exposing (getDataFromItem)
import View.Widget.GenericItemCard as GenericItemCard


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
    div []
        [ div [ css [ Tw.flex, Tw.flex_col ] ] <| List.map (viewCriterium itemPageModel.form.criteria) itemPageModel.form.order
        , div [] [ Maybe.withDefault (text "") maybeParentCriterium ]
        ]


viewItemPage : Dict Int ItemPageItem -> ItemPageModel -> Model -> Html Msg
viewItemPage items itemPageModel model =
    div []
        [ viewAddItemButton itemPageModel
            model
        , div
            [ css [ Tw.grid, Tw.grid_cols_2 ] ]
            [ viewItemList items itemPageModel model
            , sidePanelView itemPageModel model
            ]
        ]


sidePanelView : ItemPageModel -> Model -> Html Msg
sidePanelView itemPageModel model =
    case itemPageModel.form.formState of
        Hidden ->
            div [ css [ Tw.flex, Tw.justify_center ] ] [ GenericItemCard.view itemPageModel model ]

        _ ->
            viewItemForm itemPageModel model


viewAddItemButton : ItemPageModel -> Model -> Html Msg
viewAddItemButton itemPageModel model =
    let
        addButton =
            button [ onClick (ItemPage itemPageModel.itemType CreateNewItem) ] [ text "New" ]

        closeButton =
            button [ onClick (ItemPage itemPageModel.itemType CloseForm) ] [ text "Close" ]

        saveButton =
            button [ onClick (SaveItemRequested itemPageModel.itemType) ] [ text "Save" ]
    in
    case itemPageModel.form.formState of
        Model.Hidden ->
            div [] [ addButton ]

        _ ->
            div []
                [ saveButton
                , closeButton
                ]


viewItemList : Dict Int ItemPageItem -> ItemPageModel -> Model -> Html Msg
viewItemList items itemPageModel model =
    ul [] <|
        List.map (\item -> li [ onClick <| ItemPage itemPageModel.itemType (SelectItem item.id) ] [ text item.identifier ]) <|
            Dict.values items
