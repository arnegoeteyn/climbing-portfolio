module View.Page.Ascents exposing (..)

import Data exposing (Ascent, ItemPageItem)
import Html.Styled exposing (Html)
import Message exposing (ClimbingRouteMsg(..), Item(..), Msg(..), SectorMsg(..))
import Model exposing (Model)
import Utilities.ItemPageUtilities exposing (getDataFromItem)
import View.Page.GenericItemPage exposing (viewItemPage)


view : Model -> Html Msg
view model =
    viewItemPage
        (getDataFromItem AscentItem model)
        model.ascentsModel
        model
