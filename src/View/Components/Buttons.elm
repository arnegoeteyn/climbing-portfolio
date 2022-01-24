module View.Components.Buttons exposing (..)

import Css exposing (Style)
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)
import Tailwind.Utilities as Tw


cardButtonTWProperties : List Style
cardButtonTWProperties =
    [ Tw.py_2, Tw.px_4, Tw.mt_5, Tw.rounded_lg, Tw.text_white, Tw.font_semibold ]


neutralButtonTWProperties : Attribute msg
neutralButtonTWProperties =
    css <| Tw.bg_blue_300 :: cardButtonTWProperties


positiveButtonTWProperties : Attribute msg
positiveButtonTWProperties =
    css <| Tw.bg_green_500 :: cardButtonTWProperties


negativeButtonTWProperties : Attribute msg
negativeButtonTWProperties =
    css <| Tw.bg_red_500 :: cardButtonTWProperties
