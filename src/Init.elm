module Init exposing (..)

import Browser.Navigation exposing (Key)
import Data exposing (jsonFileDecoder)
import DatePicker
import Dict
import Json.Decode exposing (decodeString)
import Message exposing (Item(..), ItemRelation, Msg(..), Route(..))
import Model exposing (FormState(..), ItemPageItemForm, ItemPageModel, Model)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query
import Utilities.ItemFormUtilities as ItemFormUtilities
import Utilities.ItemPageUtilities exposing (paramsFromRoute)


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

        ( datePicker, datePickerCmd ) =
            DatePicker.init

        decodedStorage =
            decodeString jsonFileDecoder storageCache

        jsonFile =
            Result.withDefault { climbingRoutes = Dict.empty, ascents = Dict.empty, sectors = Dict.empty, areas = Dict.empty } <| decodedStorage
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
      , climbingRoutesModel = climbingRoutesModel
      , homeModel = { hovering = [] }
      , sectorsModel = sectorsModel
      , ascentsModel = ascentsModel
      , areasModel = areasModel
      , datePicker = datePicker
      }
    , Cmd.batch [ Cmd.map (ToDatePicker AscentItem "") datePickerCmd, routesCmd, sectorsCmd, areasCmd, ascentsCmd ]
    )


getRelationFromItem : Item -> ItemRelation
getRelationFromItem item =
    case item of
        ClimbingRouteItem ->
            climbingRouteRelations

        AscentItem ->
            ascentRelations

        SectorItem ->
            sectorRelations

        AreaItem ->
            areaRelations


itemPageModel : Item -> Route -> ( ItemPageModel, Cmd Msg )
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

        ( selectedItem, criteria, formState ) =
            paramsFromRoute form route
    in
    ( { form = { form | criteria = criteria, formState = formState }
      , itemType = t
      , selectedItemId = selectedItem
      , filters = Dict.empty
      , sortOnColumn = Just 0
      }
    , Cmd.none
    )


climbingRouteRelations : ItemRelation
climbingRouteRelations =
    { parent = Just SectorItem
    , child = Just AscentItem
    }


climbingRouteForm : ItemPageItemForm
climbingRouteForm =
    { criteria = ItemFormUtilities.toClimbingRouteFormCriteria Nothing
    , order = [ "name", "grade", "kind", "comment" ]
    , parentId = Nothing
    , formState = Hidden
    }


ascentForm : ItemPageItemForm
ascentForm =
    { criteria = ItemFormUtilities.toAscentFormCriteria Nothing
    , order = [ "date", "comment", "kind" ]
    , parentId = Nothing
    , formState = Hidden
    }


ascentRelations : ItemRelation
ascentRelations =
    { parent = Just ClimbingRouteItem
    , child = Nothing
    }


sectorForm : ItemPageItemForm
sectorForm =
    { criteria = ItemFormUtilities.toSectorFormCriteria Nothing
    , order = [ "name" ]
    , parentId = Nothing
    , formState = Hidden
    }


sectorRelations : ItemRelation
sectorRelations =
    { parent = Just AreaItem
    , child = Just ClimbingRouteItem
    }


areaForm : ItemPageItemForm
areaForm =
    { criteria = ItemFormUtilities.toAreaFormCriteria Nothing
    , order = [ "name", "country" ]
    , parentId = Nothing
    , formState = Hidden
    }


areaRelations : ItemRelation
areaRelations =
    { parent = Nothing
    , child = Just SectorItem
    }


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map HomeRoute Parser.top
        , Parser.map RoutesRoute (Parser.s "routes" <?> Query.int "selected" <?> Query.string "criteria")
        , Parser.map AscentsRoute (Parser.s "ascents" <?> Query.int "selected" <?> Query.string "criteria")
        , Parser.map SectorsRoute (Parser.s "sectors" <?> Query.int "selected" <?> Query.string "criteria")
        , Parser.map AreasRoute (Parser.s "areas" <?> Query.int "selected" <?> Query.string "criteria")
        ]


parseUrl : Url -> Route
parseUrl url =
    url
        |> Parser.parse routeParser
        |> Maybe.withDefault NotFoundRoute
