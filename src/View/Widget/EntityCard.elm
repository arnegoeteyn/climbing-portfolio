module View.Widget.EntityCard exposing (..)

import Data exposing (ItemPageItem)
import Dict
import Html.Styled exposing (Html, a, button, div, footer, header, img, p, text)
import Html.Styled.Attributes exposing (css, href)
import Html.Styled.Events exposing (onClick)
import Init exposing (itemPageModel)
import Message exposing (ItemPageMsg(..), ItemType(..), Msg(..))
import Model exposing (ItemPageModel, Model)
import Set
import Tailwind.Utilities as Tw
import Utilities.EntityPageUtilities as ItemPageUtilities
import Utilities.EntityUtilities as EntityUtilities
import View.Components.Buttons as Buttons


view : ItemPageModel -> Model -> Html Msg
view itemPageModel model =
    case itemPageModel.selectedItemId of
        Nothing ->
            text "Nothing selected"

        Just selectedItemId ->
            let
                items =
                    ItemPageUtilities.getDataFromItem itemPageModel.itemType model

                maybeItem =
                    Dict.get selectedItemId items
            in
            case maybeItem of
                Nothing ->
                    --should never happen
                    text "Selected Item not found."

                Just item ->
                    let
                        maybeParent =
                            EntityUtilities.getParent itemPageModel.itemType
                                |> Maybe.map (\parent -> ItemPageUtilities.getDataFromItem parent model)
                                |> Maybe.map2 (\parentId parentItems -> Dict.get parentId parentItems) item.parentId
                                |> Maybe.andThen identity

                        cardTailwindProperties =
                            css [ Tw.rounded_xl, Tw.shadow_lg, Tw.bg_purple_100 ]

                        cardHeaderImageTWProperties =
                            css [ Tw.rounded_t_lg, Tw.h_60, Tw.w_full, Tw.object_cover ]

                        cardHeaderTextTWProperties =
                            css [ Tw.text_xl, Tw.font_extrabold, Tw.px_4 ]

                        cardContentTWProperties =
                            css [ Tw.px_5 ]

                        cardDescriptionTWProperties =
                            css [ Tw.px_4, Tw.text_gray_400 ]

                        cardAreaDescriptionTWProperties =
                            css [ Tw.text_xs, Tw.text_left, Tw.text_gray_400 ]

                        cardFooterTWProperties =
                            css [ Tw.text_right, Tw.py_3, Tw.px_8, Tw.text_gray_400 ]
                    in
                    div [ cardTailwindProperties ]
                        [ img
                            [ cardHeaderImageTWProperties
                            , Html.Styled.Attributes.src "https://www.barcelona-tourist-guide.com/images/ext/attractions/montserrat/L550/montserrat-barcelona-29.jpg"
                            ]
                            []
                        , header [ cardHeaderTextTWProperties ] [ text <| item.cardHeader ]
                        , div [ cardContentTWProperties ]
                            [ p [ cardAreaDescriptionTWProperties ] [ text <| (Maybe.map (\parent -> parent.identifier) maybeParent |> Maybe.withDefault "") ]
                            , p [ cardDescriptionTWProperties ] [ text <| Maybe.withDefault "" item.cardDescription ]
                            , viewChildren item itemPageModel.itemType model
                            ]
                        , footer [ cardFooterTWProperties ]
                            [ button [ Buttons.negativeButtonTWProperties, onClick <| DeleteItem itemPageModel.itemType item.id ] [ text "delete" ]
                            , button [ Buttons.positiveButtonTWProperties, onClick (ItemPage itemPageModel.itemType <| UpdateItem item.id) ] [ text "edit" ]
                            ]
                        ]


viewChildren : ItemPageItem -> ItemType -> Model -> Html Msg
viewChildren itemPageItem item model =
    div []
        [ case EntityUtilities.getChild item of
            Just childItemType ->
                let
                    childCollection =
                        ItemPageUtilities.getDataFromItem childItemType model

                    children =
                        List.sortBy .identifier <| List.filterMap identity <| List.map (\id -> Dict.get id childCollection) <| Set.toList <| Maybe.withDefault Set.empty itemPageItem.childIds

                    viewChild child =
                        div []
                            [ a
                                [ onClick <| ItemPage childItemType (SelectItem child.id)
                                , href <| ItemPageUtilities.urlToItem childItemType child.id
                                ]
                                [ text child.identifier ]
                            ]
                in
                div []
                    [ div [] <| List.map viewChild children
                    ]

            Nothing ->
                div [] []
        , EntityUtilities.getChild item
            |> Maybe.map
                (\child ->
                    a
                        [ href <|
                            ItemPageUtilities.urlToCreateItem child [ { key = "_parentId", value = String.fromInt itemPageItem.id } ]
                        ]
                        [ text "addChild" ]
                )
            |> Maybe.withDefault (text "")
        ]