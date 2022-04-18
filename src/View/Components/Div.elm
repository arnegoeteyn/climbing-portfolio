module View.Components.Div exposing (..)

import Html.Styled exposing (Attribute)
import Html.Styled.Attributes exposing (css)
import Tailwind.Utilities as Tw


importantProperties : Attribute msg
importantProperties =
    css [ Tw.font_bold ]


largeProperties : Attribute msg
largeProperties =
    css [ Tw.text_lg ]
