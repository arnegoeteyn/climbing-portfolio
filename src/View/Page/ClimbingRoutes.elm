module View.Page.ClimbingRoutes exposing (..)

import Html.Styled exposing (Html)
import Message exposing (ClimbingRouteMsg(..), Item(..), Msg(..))
import Model exposing (Model)
import Utilities.ItemPageUtilities exposing (getDataFromItem)
import View.Page.GenericItemPage exposing (viewItemPage)


viewClimbingRoutes : Model -> Html Msg
viewClimbingRoutes model =
    viewItemPage
        (getDataFromItem ClimbingRouteItem model)
        model.climbingRoutesModel
        model



-- viewClimbingRoute : ClimbingRoute -> Model -> Html Msg
-- viewClimbingRoute route _ =
--     li
--         [ onClick <| Message.ClimbingRoute <| Message.ClimbingRouteSelected route ]
--         [ text <| route.name ++ " " ++ route.grade ]
