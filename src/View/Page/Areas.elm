module View.Page.Areas exposing (..)

import Html.Styled exposing (Html)
import Message exposing (Item(..), Msg)
import Model exposing (Model)
import Utilities.ItemPageUtilities exposing (getDataFromItem)
import View.Page.GenericItemPage exposing (viewItemPage)


view : Model -> Html Msg
view model =
    viewItemPage
        (getDataFromItem AreaItem model)
        model.areasModel
        model
