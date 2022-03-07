module View.Page.EntityPage exposing (..)

import Dict exposing (Dict(..))
import Html.Styled exposing (Html, button, div, h2, table, td, text, tr)
import Html.Styled.Attributes as A exposing (value)
import Html.Styled.Events exposing (onClick)
import Message exposing (CriteriumUpdate(..), ItemPageMsg(..), ItemType, Msg(..))
import Model exposing (FormState(..), Model)
import Svg.Styled.Attributes exposing (css)
import Tailwind.Breakpoints as B
import Tailwind.Utilities as Tw
import Utilities exposing (viewInput)
import Utilities.EntityFormUtilities exposing (getFormFromItem)
import Utilities.EntityPageUtilities as EntityPageUtilities
import Utilities.EntityUtilities as EntityUtilities
import View.Components.Table as Table
import View.Widget.EntityCard as GenericItemCard
import View.Widget.EntityForm as ItemForm


viewEntityPage : ItemType -> Model -> Html Msg
viewEntityPage type_ model =
    let
        sidePaneCss =
            [ Tw.h_full, Tw.flex, Tw.max_h_screen, Tw.overflow_y_auto, Tw.flex_col, Tw.flex_grow ]

        hasASelection =
            EntityPageUtilities.hasASelection type_ model
    in
    div []
        [ viewAddItemButton type_ model
        , div
            [ css [ B.lg [ Tw.flex, Tw.flex_row, Tw.h_screen ], Tw.block ] ]
            [ div
                [ css
                    (B.lg (sidePaneCss ++ [ Tw.w_4over6 ])
                        :: Utilities.filterList [ ( Tw.hidden, not hasASelection ) ]
                    )
                ]
                [ viewItemList type_ model ]
            , div [ css (sidePaneCss ++ [ B.lg [ Tw.w_2over6 ] ]) ] [ sidePanelView type_ model ]
            ]
        ]


sidePanelView : ItemType -> Model -> Html Msg
sidePanelView type_ model =
    let
        form =
            getFormFromItem type_ model
    in
    case form.formState of
        Hidden ->
            div [ css [ Tw.flex, Tw.justify_center ] ] [ GenericItemCard.view type_ model ]

        _ ->
            ItemForm.view form model


viewAddItemButton : ItemType -> Model -> Html Msg
viewAddItemButton type_ model =
    let
        addButton =
            button [ onClick (ItemPage type_ CreateNewItem) ] [ text "New" ]

        closeButton =
            button [ onClick (ItemPage type_ CloseForm) ] [ text "Close" ]

        saveButton =
            button [ onClick (SaveItemRequested type_) ] [ text "Save" ]
    in
    case getFormFromItem type_ model |> .formState of
        Model.Hidden ->
            div [] [ addButton ]

        _ ->
            div []
                [ saveButton
                , closeButton
                ]


viewItemList : ItemType -> Model -> Html Msg
viewItemList type_ model =
    let
        filteredItems =
            List.filter
                (\id ->
                    Dict.foldl
                        (\key value acc -> acc && EntityUtilities.matchesFilter type_ id key value model)
                        True
                        (EntityPageUtilities.activeFilters type_ model)
                )
            <|
                EntityPageUtilities.sortedItems type_ model

        headers =
            EntityPageUtilities.entityPageTableHeaders type_
    in
    div []
        [ h2 [] [ text <| String.fromInt (List.length filteredItems) ++ " items" ]
        , table
            [ Table.tableProperties ]
          <|
            [ Html.Styled.thead [ Table.tableHeaderProperties ] <| List.map (\header -> td [] [ text header ]) headers
            , Html.Styled.thead [ Table.tableHeaderProperties ] <|
                List.map
                    (\header ->
                        td []
                            [ viewInput "text"
                                header
                                (Maybe.withDefault "" <| Dict.get header (EntityPageUtilities.activeFilters type_ model))
                                (\value -> ItemPage type_ <| FilterUpdateMessage header value)
                            ]
                    )
                    headers
            , Html.Styled.tbody [ Table.tableBodyProperties ] <|
                List.map
                    (\id ->
                        tr
                            ((onClick <| ItemPage type_ (SelectItem <| Just id))
                                :: Utilities.filterAndReplaceList [ ( Table.selectedRowProperties, EntityPageUtilities.selectedItemId type_ model == Just id, Nothing ) ]
                            )
                        <|
                            List.map
                                (\( _, value ) -> td [] [ text value ])
                                (EntityPageUtilities.tableValues type_ id model)
                    )
                <|
                    filteredItems
            ]
        ]
