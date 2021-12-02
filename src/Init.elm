module Init exposing (..)

import Browser.Navigation exposing (Key)
import DatePicker
import Dict
import Message exposing (ClimbingRouteMsg(..), Msg, Route(..))
import Model exposing (ClimbingRouteForm, ClimbingRoutesModel, Model)
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


init : Url -> Key -> ( Model, Cmd Msg )
init url key =
    let
        ( climbingRoutesModel, routesCmd ) =
            routesModel
    in
    ( { appState = Model.NotReady
      , url = url
      , route = parseUrl url
      , key = key
      , climbingRoutes = Dict.empty
      , ascents = Dict.empty
      , sectors = Dict.empty
      , climbingRoutesModel = climbingRoutesModel
      }
    , routesCmd
    )


routesModel : ( ClimbingRoutesModel, Cmd Msg )
routesModel =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
    ( { selectedRoute = Nothing
      , form = Nothing
      , showNewAscentDate = False
      , datePicker = datePicker
      , date = Nothing
      }
    , Cmd.map Message.ToDatePicker datePickerFx
    )


initClimbingRouteForm : ClimbingRouteForm
initClimbingRouteForm =
    { name = ""
    , grade = ""
    }


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map HomeRoute Parser.top
        , Parser.map RoutesRoute (Parser.s "routes")
        , Parser.map AscentsRoute (Parser.s "ascents")
        , Parser.map SectorsRoute (Parser.s "sectors")
        ]


parseUrl : Url -> Route
parseUrl url =
    url
        |> Parser.parse routeParser
        |> Maybe.withDefault NotFoundRoute
