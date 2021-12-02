module View.Page.Home exposing (viewHome)

import Html.Styled exposing (Html, text)
import Message exposing (Msg)
import Model exposing (Model)


viewHome : Model -> Html Msg
viewHome model =
    text "Statistiekjes"
