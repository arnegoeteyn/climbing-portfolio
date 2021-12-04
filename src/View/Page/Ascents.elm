module View.Page.Ascents exposing (..)

import Data exposing (Ascent, ItemPageItem)
import Dict
import Html.Styled exposing (Html)
import Message exposing (ClimbingRouteMsg(..), Msg(..), SectorMsg(..))
import Model exposing (Model)
import View.Page.GenericItemPage exposing (viewItemPage)


view : Model -> Html Msg
view model =
    viewItemPage
        (Dict.map toAscentItem model.ascents)
        model.ascentsModel
        model


toAscentItem : Int -> Ascent -> ItemPageItem
toAscentItem _ ascent =
    { cardHeader = ascent.date
    , identifier = ascent.date
    , id = ascent.id
    }
