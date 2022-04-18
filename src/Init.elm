module Init exposing (..)

import Browser.Navigation exposing (Key)
import Data exposing (jsonFileDecoder)
import DatePicker
import Dict
import Json.Decode exposing (decodeString)
import Message exposing (ItemType(..), Msg(..), Route(..))
import Model exposing (EntityForm, FormState(..), ItemPageModel, Model)
import Select
import Set
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query
import Utilities.EntityFormUtilities as EntityFormUtilities
import Utilities.EntityPageUtilities as EntityPageUtilities


init : String -> Url -> Key -> ( Model, Cmd Msg )
init storageCache url key =
    let
        parsedUrl =
            parseUrl url

        ( climbingRoutesModel, routesCmd ) =
            itemPageModel ClimbingRouteItem parsedUrl

        ( sectorsModel, sectorsCmd ) =
            itemPageModel SectorItem parsedUrl

        ( ascentsModel, ascentsCmd ) =
            itemPageModel AscentItem parsedUrl

        ( areasModel, areasCmd ) =
            itemPageModel AreaItem parsedUrl

        ( tripsModel, tripsCmd ) =
            itemPageModel TripItem parsedUrl

        ( datePicker, datePickerCmd ) =
            DatePicker.init

        decodedStorage =
            decodeString jsonFileDecoder storageCache

        jsonFile =
            Result.withDefault { climbingRoutes = Dict.empty, ascents = Dict.empty, sectors = Dict.empty, areas = Dict.empty, trips = Dict.empty } <| decodedStorage
    in
    ( { appState =
            case decodedStorage of
                Result.Ok _ ->
                    Model.Ready

                Result.Err _ ->
                    -- appstate can just default to empty dictionaries
                    Model.Ready
      , url = url
      , route = parseUrl url
      , key = key
      , climbingRoutes = jsonFile.climbingRoutes
      , ascents = jsonFile.ascents
      , sectors = jsonFile.sectors
      , areas = jsonFile.areas
      , trips = jsonFile.trips
      , climbingRoutesModel = climbingRoutesModel
      , homeModel = { hovering = [] }
      , sectorsModel = sectorsModel
      , ascentsModel = ascentsModel
      , areasModel = areasModel
      , tripsModel = tripsModel
      , datePicker = datePicker
      , overviewModel =
            { routeFilter = ""
            , selected = []
            , selectState = Select.init "sectors"
            , selectedClimbingRoute = Nothing
            }
      }
    , Cmd.batch [ Cmd.map (ToDatePicker AscentItem "") datePickerCmd, routesCmd, sectorsCmd, areasCmd, ascentsCmd, tripsCmd ]
    )


itemPageModel : ItemType -> Route -> ( ItemPageModel, Cmd Msg )
itemPageModel t route =
    let
        form =
            case t of
                ClimbingRouteItem ->
                    climbingRouteForm

                AscentItem ->
                    ascentForm

                SectorItem ->
                    sectorForm

                AreaItem ->
                    areaForm

                TripItem ->
                    tripForm

        ( selectedItem, criteria, formState ) =
            EntityPageUtilities.paramsFromRoute form route
    in
    ( { form = { form | criteria = criteria, formState = formState, parentId = Dict.get "_parentId" criteria |> Maybe.map .value }
      , itemType = t
      , selectedItemId = selectedItem
      , filterValues = Dict.empty
      , sortOnColumn = Just 0
      }
    , Cmd.none
    )


climbingRouteForm : EntityForm
climbingRouteForm =
    { criteria = EntityFormUtilities.toClimbingRouteFormCriteria Nothing
    , order = [ "name", "grade", "kind", "comment" ]
    , parentId = Nothing
    , formState = Hidden
    , entity = Model.Entity ClimbingRouteItem Nothing
    }


ascentForm : EntityForm
ascentForm =
    { criteria = EntityFormUtilities.toAscentFormCriteria Nothing
    , order = [ "date", "comment", "kind" ]
    , parentId = Nothing
    , formState = Hidden
    , entity = Model.Entity AscentItem Nothing
    }


sectorForm : EntityForm
sectorForm =
    { criteria = EntityFormUtilities.toSectorFormCriteria Nothing
    , order = [ "name" ]
    , parentId = Nothing
    , formState = Hidden
    , entity = Model.Entity SectorItem Nothing
    }


areaForm : EntityForm
areaForm =
    { criteria = EntityFormUtilities.toAreaFormCriteria Nothing
    , order = [ "name", "country" ]
    , parentId = Nothing
    , formState = Hidden
    , entity = Model.Entity AreaItem Nothing
    }


tripForm : EntityForm
tripForm =
    { criteria = EntityFormUtilities.toTripFormCriteria Nothing
    , order = [ "from", "to" ]
    , parentId = Nothing
    , formState = Hidden
    , entity = Model.Entity TripItem Nothing
    }


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map HomeRoute Parser.top
        , Parser.map OverviewRoute (Parser.s "overview")
        , Parser.map RoutesRoute (Parser.s "routes" <?> Query.int "selected" <?> Query.string "criteria")
        , Parser.map AscentsRoute (Parser.s "ascents" <?> Query.int "selected" <?> Query.string "criteria")
        , Parser.map SectorsRoute (Parser.s "sectors" <?> Query.int "selected" <?> Query.string "criteria")
        , Parser.map AreasRoute (Parser.s "areas" <?> Query.int "selected" <?> Query.string "criteria")
        , Parser.map TripsRoute (Parser.s "trips" <?> Query.int "selected" <?> Query.string "criteria")
        ]


parseUrl : Url -> Route
parseUrl url =
    url
        |> Parser.parse routeParser
        |> Maybe.withDefault NotFoundRoute
