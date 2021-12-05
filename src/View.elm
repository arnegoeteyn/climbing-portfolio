module View exposing (..)

import Html exposing (Html)
import Html.Styled exposing (Html, a, button, div, h1, nav, text, toUnstyled)
import Html.Styled.Attributes exposing (css, href)
import Html.Styled.Events exposing (onClick)
import Message exposing (Msg(..), Route(..))
import Model exposing (Model)
import Tailwind.Breakpoints as B
import Tailwind.Utilities as Tw
import Utilities exposing (filterList)
import View.Page.Areas
import View.Page.Ascents
import View.Page.ClimbingRoutes exposing (viewClimbingRoutes)
import View.Page.Home exposing (viewHome)
import View.Page.Sectors


view : Model -> List (Html.Html Msg)
view model =
    List.map toUnstyled <|
        case model.appState of
            Model.Ready ->
                [ viewHeader model
                , viewPage model
                ]

            Model.NotReady ->
                [ button [ onClick JsonRequested ] [ text "Load JSON" ]
                ]


viewHeader : Model -> Html Msg
viewHeader model =
    let
        logo =
            div [ css [ Tw.flex, Tw.items_center, Tw.flex_shrink_0, Tw.text_white, Tw.mr_6 ] ] [ text "Climbing portfolio" ]

        links =
            div [ Html.Styled.Attributes.css [ Tw.w_full, Tw.block, Tw.flex_grow, B.lg [ Tw.flex, Tw.items_center, Tw.w_auto ] ] ]
                [ navLink HomeRoute { url = "/", caption = "Home" }
                , navLink AscentsRoute { url = "/ascents", caption = "Ascents" }
                , navLink RoutesRoute { url = "/routes", caption = "Routes" }
                , navLink SectorsRoute { url = "/sectors", caption = "Sectors" }
                , navLink AreasRoute { url = "/areas", caption = "Areas" }
                ]

        navAttributes =
            [ Html.Styled.Attributes.css
                [ Tw.flex
                , Tw.items_center
                , Tw.justify_between
                , Tw.flex_wrap
                , Tw.bg_purple_400
                , Tw.p_6
                ]
            ]

        isActive route =
            model.route == route

        navLink : Route -> { url : String, caption : String } -> Html msg
        navLink route { url, caption } =
            a
                [ href url
                , Html.Styled.Attributes.css <|
                    [ Tw.block, Tw.mt_4, Tw.mr_4, Tw.text_purple_200, B.lg [ Tw.inline_block, Tw.mt_0 ] ]
                        ++ filterList [ ( Tw.underline, isActive route, Just Tw.no_underline ) ]
                ]
                [ text caption ]
    in
    nav navAttributes
        [ logo
        , links
        , button [ onClick JsonRequested ] [ text "Load JSON" ]
        , button [ onClick ExportRequested ] [ text "Save JSON" ]
        ]


viewPage : Model -> Html Msg
viewPage model =
    div []
        [ case model.route of
            HomeRoute ->
                viewHome model

            RoutesRoute ->
                viewClimbingRoutes model

            AscentsRoute ->
                View.Page.Ascents.view model

            SectorsRoute ->
                View.Page.Sectors.view model

            AreasRoute ->
                View.Page.Areas.view model

            NotFoundRoute ->
                h1 [] [ text "404 :(" ]
        ]
