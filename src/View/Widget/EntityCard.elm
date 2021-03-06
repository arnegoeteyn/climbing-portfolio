module View.Widget.EntityCard exposing (..)

import Data
import Html.Styled exposing (Html, a, button, div, footer, header, img, p, table, text, tr)
import Html.Styled.Attributes exposing (css, href, type_)
import Html.Styled.Events exposing (onClick)
import Message exposing (ItemPageMsg(..), ItemType(..), Msg(..))
import Model exposing (Model)
import Set
import Tailwind.Utilities as Tw
import Utilities
import Utilities.EntityPageUtilities as ItemPageUtilities
import Utilities.EntityUtilities as EntityUtilities
import View.Components.Buttons as Buttons


view : ItemType -> Model -> Html Msg
view type_ model =
    case ItemPageUtilities.selectedItemId type_ model of
        Nothing ->
            text "Nothing selected"

        Just itemId ->
            let
                maybeParent =
                    EntityUtilities.getParent type_ itemId model

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
                , header [ cardHeaderTextTWProperties ] [ viewCardTitle type_ itemId model ]
                , div [ cardContentTWProperties ]
                    [ p [ cardAreaDescriptionTWProperties ] [ Maybe.map2 (\t p -> viewCardTitle t p model) (EntityUtilities.getParentType type_) maybeParent |> Maybe.withDefault (text "") ]
                    , p [ cardDescriptionTWProperties ] [ viewCardDescription type_ itemId model ]
                    , viewChildren type_ itemId model
                    , viewAddChildLink type_ itemId model
                    ]
                , footer [ cardFooterTWProperties ]
                    [ button [ Buttons.neutralButtonTWProperties, onClick <| (ItemPage type_ <| SelectItem Nothing) ] [ text "close" ]
                    , button [ Buttons.negativeButtonTWProperties, onClick <| DeleteItem type_ itemId ] [ text "delete" ]
                    , button [ Buttons.positiveButtonTWProperties, onClick (ItemPage type_ <| UpdateItem itemId) ] [ text "edit" ]
                    ]
                ]


viewCardTitle : ItemType -> Int -> Model -> Html Msg
viewCardTitle type_ id model =
    Maybe.withDefault (text "") <|
        case type_ of
            AreaItem ->
                EntityUtilities.getArea model id |> Maybe.map (.name >> text)

            SectorItem ->
                EntityUtilities.getSector model id |> Maybe.map (.name >> text)

            ClimbingRouteItem ->
                EntityUtilities.getClimbingRoute model id |> Maybe.map ((\c -> Utilities.stringFromList [ c.name, " [", c.grade, "]" ]) >> text)

            AscentItem ->
                let
                    dateOrEmpty ascent =
                        Utilities.maybeDateToString ascent.date
                in
                EntityUtilities.getAscent id model |> Maybe.map ((\a -> Utilities.stringFromList [ dateOrEmpty a, " [", Data.ascentKindToString a.kind, "]" ]) >> text)

            TripItem ->
                EntityUtilities.getTrip id model |> Maybe.map tripTitle


viewCardDescription : ItemType -> Int -> Model -> Html Msg
viewCardDescription type_ id model =
    Maybe.withDefault (text "") <|
        case type_ of
            AreaItem ->
                EntityUtilities.getArea model id |> Maybe.map (.country >> text)

            SectorItem ->
                Nothing

            ClimbingRouteItem ->
                EntityUtilities.getClimbingRoute model id |> Maybe.andThen .comment |> Maybe.map text

            AscentItem ->
                EntityUtilities.getAscent id model |> Maybe.andThen .comment |> Maybe.map text

            TripItem ->
                EntityUtilities.getTrip id model |> Maybe.map (tripDescription model)


viewChildren : ItemType -> Int -> Model -> Html Msg
viewChildren type_ id model =
    div [] <|
        case EntityUtilities.getChildType type_ of
            Just childItemType ->
                let
                    children =
                        EntityUtilities.getChildren type_ id model |> Set.toList |> Utilities.sortByDescending (\i -> EntityUtilities.sortEntityBy childItemType i model)
                in
                List.map (\child -> viewLink childItemType child model) children

            Nothing ->
                []


viewLink : ItemType -> Int -> Model -> Html Msg
viewLink type_ id model =
    div []
        [ a
            [ onClick <| ItemPage type_ (SelectItem <| Just id)
            , href <| ItemPageUtilities.urlToItem type_ id
            ]
            [ viewCardTitle type_ id model ]
        ]


viewAddChildLink : ItemType -> Int -> Model -> Html Msg
viewAddChildLink type_ id _ =
    EntityUtilities.getChildType type_
        |> Maybe.map
            (\child ->
                a
                    [ href <|
                        ItemPageUtilities.urlToCreateItem child [ { key = "_parentId", value = String.fromInt id } ]
                    ]
                    [ text "addChild" ]
            )
        |> Maybe.withDefault (text "")



--| Trips


areasFromTrip : Model -> Data.Trip -> Html msg
areasFromTrip model trip =
    text <| Utilities.stringFromListWith " - " (EntityUtilities.areasFromTrip model trip |> List.map .name)


tripDescription : Model -> Data.Trip -> Html msg
tripDescription model trip =
    let
        renderGrade ( grade, amount ) =
            tr []
                [ Html.Styled.td [] [ text grade ]
                , Html.Styled.td [] [ text <| String.fromInt amount ]
                ]
    in
    div []
        [ areasFromTrip model trip
        , table [] <|
            List.map renderGrade (EntityUtilities.groupedRoutesFromTrip trip model)
        ]


tripTitle : Data.Trip -> Html msg
tripTitle =
    EntityUtilities.tripTitle >> text
