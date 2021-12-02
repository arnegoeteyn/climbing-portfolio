module View.Widget.ClimbingRouteCard exposing (..)

import Data exposing (ClimbingRoute)
import DatePicker
import Dict
import Html.Styled exposing (Html, button, div, footer, header, img, p, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Message exposing (ClimbingRouteMsg(..), Msg)
import Model exposing (Model)
import Tailwind.Utilities as Tw


viewClimbingRouteCard : Maybe ClimbingRoute -> Model -> Html Msg
viewClimbingRouteCard maybeClimbingRoute model =
    case maybeClimbingRoute of
        Nothing ->
            text "Nothing selected"

        Just climbingRoute ->
            let
                maybeSector =
                    Maybe.andThen (\x -> Dict.get x model.sectors) climbingRoute.sectorId

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

                cardButtonTWProperties =
                    css [ Tw.py_2, Tw.px_4, Tw.mt_5, Tw.bg_green_500, Tw.rounded_lg, Tw.text_white, Tw.font_semibold ]
            in
            div [ cardTailwindProperties ]
                [ img
                    [ cardHeaderImageTWProperties
                    , Html.Styled.Attributes.src "https://www.barcelona-tourist-guide.com/images/ext/attractions/montserrat/L550/montserrat-barcelona-29.jpg"
                    ]
                    []
                , header [ cardHeaderTextTWProperties ] [ text <| climbingRoute.name ++ " [" ++ climbingRoute.grade ++ "]" ]
                , div [ cardContentTWProperties ]
                    [ p [ cardAreaDescriptionTWProperties ] [ text <| (Maybe.map (\sector -> sector.name ++ " ~ Fontainebleau") maybeSector |> Maybe.withDefault "") ]
                    , p [ cardDescriptionTWProperties ] [ text <| Maybe.withDefault "" climbingRoute.description ]
                    , viewAscents climbingRoute model
                    ]
                , footer [ cardFooterTWProperties ] [ button [ cardButtonTWProperties ] [ text "edit" ] ]
                ]


viewAscents : ClimbingRoute -> Model -> Html Msg
viewAscents route model =
    let
        ascents =
            List.filterMap identity <| List.map (\id -> Dict.get id model.ascents) (Maybe.withDefault [] route.ascentIds)

        viewAscent ascent =
            div [] [ text ascent.date ]

        datePickerDialog =
            DatePicker.view model.climbingRoutesModel.date DatePicker.defaultSettings model.climbingRoutesModel.datePicker
                |> Html.Styled.fromUnstyled
                |> Html.Styled.map Message.ToDatePicker
    in
    div []
        [ text <| String.fromInt (List.length ascents) ++ " ascents!"
        , button [ onClick (Message.ClimbingRoute <| AddAscentButtonClicked) ] [ text "+" ]
        , if model.climbingRoutesModel.showNewAscentDate then
            datePickerDialog

          else
            text ""
        , div [] <| List.map viewAscent ascents
        ]
