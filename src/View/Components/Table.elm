module View.Components.Table exposing (..)

import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)
import Tailwind.Utilities as Tw


tableProperties : Attribute msg
tableProperties =
    css [ Tw.table_auto, Tw.w_full ]


tableHeaderProperties : Attribute msg
tableHeaderProperties =
    css [ Tw.text_xs, Tw.font_semibold, Tw.uppercase, Tw.text_gray_400, Tw.bg_gray_50 ]


tableBodyProperties : Attribute msg
tableBodyProperties =
    css [ Tw.text_sm, Tw.divide_y, Tw.divide_gray_100 ]
