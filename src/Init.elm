module Init exposing (..)

import Browser.Navigation exposing (Key)
import Data exposing (jsonFileDecoder)
import DatePicker
import Dict
import Json.Decode exposing (decodeString)
import Message exposing (ClimbingRouteMsg(..), Msg, Route(..))
import Model exposing (ClimbingRouteForm, ClimbingRoutesModel, Model, SectorsModel)
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


init : String -> Url -> Key -> ( Model, Cmd Msg )
init storageCache url key =
    let
        ( climbingRoutesModel, routesCmd ) =
            routesModel

        ( aSectorsModel, sectorsCmd ) =
            sectorsModel

        decodedStorage =
            decodeString jsonFileDecoder storageCache

        jsonFile =
            Result.withDefault { climbingRoutes = Dict.empty, ascents = Dict.empty, sectors = Dict.empty } <| decodedStorage
    in
    ( { appState =
            case decodedStorage of
                Result.Ok _ ->
                    Model.Ready

                Result.Err _ ->
                    Model.NotReady
      , url = url
      , route = parseUrl url
      , key = key
      , climbingRoutes = jsonFile.climbingRoutes
      , ascents = jsonFile.ascents
      , sectors = jsonFile.sectors
      , climbingRoutesModel = climbingRoutesModel
      , sectorsModel = aSectorsModel
      }
    , Cmd.batch [ routesCmd ]
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


sectorsModel : ( SectorsModel, Cmd Msg )
sectorsModel =
    ( { selectedSector = Nothing }, Cmd.none )


initClimbingRouteForm : ClimbingRouteForm
initClimbingRouteForm =
    { name = ""
    , grade = ""
    , sectorId = ""
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
