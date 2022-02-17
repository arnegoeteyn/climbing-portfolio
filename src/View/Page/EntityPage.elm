module View.Page.EntityPage exposing (..)

import Data exposing (ItemPageItem)
import Dict exposing (Dict(..))
import Html.Styled exposing (Html, button, div, h2, table, td, text, tr)
import Html.Styled.Attributes exposing (value)
import Html.Styled.Events exposing (onClick)
import Message exposing (CriteriumUpdate(..), ItemPageMsg(..), ItemType, Msg(..))
import Model exposing (FormState(..), ItemPageModel, Model)
import Svg.Styled.Attributes exposing (css)
import Tailwind.Utilities as Tw
import Utilities exposing (viewInput)
import Utilities.EntityPageUtilities as EntityPageUtilities
import View.Components.Table as Table
import View.Widget.EntityCard as GenericItemCard
import View.Widget.EntityForm as ItemForm


view : ItemType -> Model -> Html Msg
view item model =
    let
        itemPageModel =
            EntityPageUtilities.getModelFromItem item model

        items =
            EntityPageUtilities.sortedItems itemPageModel model
    in
    div []
        [ viewAddItemButton itemPageModel
            model
        , div
            [ css [ Tw.flex, Tw.flex_row, Tw.h_screen ] ]
            [ div [ css [ Tw.h_full, Tw.flex, Tw.max_h_screen, Tw.overflow_y_auto, Tw.flex_col, Tw.flex_grow ] ] [ viewItemList items itemPageModel model ]
            , div [ css [ Tw.h_full, Tw.flex, Tw.max_h_screen, Tw.overflow_y_auto, Tw.flex_col, Tw.flex_grow ] ] [ sidePanelView itemPageModel model ]
            ]
        ]


sidePanelView : ItemPageModel -> Model -> Html Msg
sidePanelView itemPageModel model =
    case itemPageModel.form.formState of
        Hidden ->
            div [ css [ Tw.flex, Tw.justify_center ] ] [ GenericItemCard.view itemPageModel.itemType itemPageModel.selectedItemId model ]

        _ ->
            ItemForm.view itemPageModel model


viewAddItemButton : ItemPageModel -> Model -> Html Msg
viewAddItemButton itemPageModel _ =
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


viewItemList : List ItemPageItem -> ItemPageModel -> Model -> Html Msg
viewItemList items itemPageModel _ =
    let
        headers =
            List.head items |> Maybe.map .tableValues |> Maybe.withDefault []

        filteredItems =
            List.filter
                (\item ->
                    let
                        tableValuesDict =
                            Dict.fromList <| item.tableValues
                    in
                    Dict.foldl
                        (\key value acc ->
                            acc && String.contains (String.toLower value) (String.toLower <| Maybe.withDefault "" <| Dict.get key tableValuesDict)
                        )
                        True
                        itemPageModel.filters
                )
                items
    in
    div []
        [ h2 [] [ text <| String.fromInt (List.length filteredItems) ++ " items" ]
        , table
            [ Table.tableProperties ]
          <|
            [ Html.Styled.thead [ Table.tableHeaderProperties ] <| List.map (\( header, _ ) -> td [] [ text header ]) headers
            , Html.Styled.thead [ Table.tableHeaderProperties ] <|
                List.map
                    (\( header, _ ) ->
                        td []
                            [ viewInput "text"
                                header
                                (Maybe.withDefault "" <| Dict.get header itemPageModel.filters)
                                (\value -> ItemPage itemPageModel.itemType <| FilterUpdateMessage header value)
                            ]
                    )
                    headers
            , Html.Styled.tbody [ Table.tableBodyProperties ] <|
                List.map
                    (\item ->
                        tr
                            ((onClick <| ItemPage itemPageModel.itemType (SelectItem item.id))
                                :: Utilities.filterList [ ( Table.selectedRowProperties, itemPageModel.selectedItemId == Just item.id, Nothing ) ]
                            )
                        <|
                            List.map
                                (\( _, value ) -> td [] [ text value ])
                                item.tableValues
                    )
                <|
                    filteredItems
            ]
        ]
