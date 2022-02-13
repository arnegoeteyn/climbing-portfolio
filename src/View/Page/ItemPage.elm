module View.Page.ItemPage exposing (..)

import Data exposing (ItemPageItem)
import Date
import DatePicker
import Dict exposing (Dict(..))
import Html
import Html.Styled exposing (Html, button, div, h2, option, select, table, td, text, tr)
import Html.Styled.Attributes exposing (value)
import Html.Styled.Events exposing (onClick, onInput)
import Init
import Message exposing (CriteriumUpdate(..), Item, ItemPageMsg(..), Msg(..))
import Model exposing (FormState(..), ItemPageModel, Model)
import Svg.Styled.Attributes exposing (css)
import Tailwind.Utilities as Tw
import Utilities exposing (viewInput)
import Utilities.ItemPageUtilities as ItemPageUtilities
import View.Components.Table as Table
import View.Widget.ItemCard as GenericItemCard


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
                    case criterium.type_ of
                        Model.String ->
                            viewInput "text" criterium.label criterium.value (\value -> ItemPage itemPageModel.itemType (FormUpdateMessage <| UpdateKey key value))

                        Model.Enumeration options ->
                            select
                                [ onInput (\value -> ItemPage itemPageModel.itemType (FormUpdateMessage <| UpdateKey key value))
                                ]
                            <|
                                (options
                                    |> List.map
                                        (\item ->
                                            option
                                                [ value item
                                                , Html.Styled.Attributes.selected <| item == (Maybe.withDefault "" <| Maybe.map .value <| Dict.get key itemPageModel.form.criteria)
                                                ]
                                                [ text item ]
                                        )
                                )

                        Model.Date ->
                            DatePicker.view
                                (if String.isEmpty criterium.value then
                                    Nothing

                                 else
                                    Date.fromIsoString criterium.value |> Result.toMaybe
                                )
                                DatePicker.defaultSettings
                                model.datePicker
                                |> Html.map (ToDatePicker itemPageModel.itemType key)
                                |> Html.Styled.fromUnstyled

        maybeParentCriterium =
            Init.getRelationFromItem itemPageModel.itemType
                |> .parent
                |> Maybe.map
                    (\parentItem ->
                        select
                            [ onInput (\value -> ItemPage itemPageModel.itemType (FormUpdateMessage <| UpdateParent value))
                            ]
                        <|
                            option [ value "" ] [ text "" ]
                                :: (ItemPageUtilities.sortedItems (ItemPageUtilities.getModelFromItem parentItem model) model
                                        |> List.map
                                            (\item ->
                                                option
                                                    [ value <| String.fromInt item.id
                                                    , Html.Styled.Attributes.selected <| String.fromInt item.id == (Maybe.withDefault "" <| Maybe.map .value <| Dict.get "_parentId" itemPageModel.form.criteria)
                                                    ]
                                                    [ text item.identifier ]
                                            )
                                   )
                    )
    in
    div []
        [ div [ css [ Tw.flex, Tw.flex_col ] ] <| List.map (viewCriterium itemPageModel.form.criteria) itemPageModel.form.order
        , div [] [ Maybe.withDefault (text "") maybeParentCriterium ]
        ]


viewItemPage : Item -> Model -> Html Msg
viewItemPage item model =
    let
        itemPageModel =
            ItemPageUtilities.getModelFromItem item model

        items =
            ItemPageUtilities.sortedItems itemPageModel model
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
            div [ css [ Tw.flex, Tw.justify_center ] ] [ GenericItemCard.view itemPageModel model ]

        _ ->
            viewItemForm itemPageModel model


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
                            [ onClick <| ItemPage itemPageModel.itemType (SelectItem item.id)
                            ]
                        <|
                            List.map
                                (\( _, value ) -> td [] [ text value ])
                                item.tableValues
                    )
                <|
                    filteredItems
            ]
        ]