module View.Page.Sectors exposing (view)

import Data exposing (ItemPageItem, Sector)
import Dict
import Html.Styled exposing (Html, button, div, li, option, select, text, ul)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onClick, onInput)
import Message exposing (ClimbingRouteMsg(..), Item(..), Msg(..), SectorMsg(..))
import Model exposing (Model)
import Utilities exposing (viewInput)
import Utilities.ItemPageUtilities exposing (getDataFromItem)
import View.Page.GenericItemPage exposing (viewItemPage)


view : Model -> Html Msg
view model =
    viewItemPage
        (getDataFromItem SectorItem model)
        model.sectorsModel
        model
