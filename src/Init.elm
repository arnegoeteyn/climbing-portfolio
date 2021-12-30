module Init exposing (..)

import Browser.Navigation exposing (Key)
import Data exposing (jsonFileDecoder)
import DatePicker exposing (DatePicker)
import Dict
import Html.Attributes exposing (type_)
import Json.Decode exposing (decodeString)
import Message exposing (ClimbingRouteMsg(..), Item(..), ItemRelation, Msg(..), Route(..))
import Model exposing (FormState(..), ItemPageItemForm, ItemPageModel, Model)
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)


init : String -> Url -> Key -> ( Model, Cmd Msg )
init storageCache url key =
    let
        ( climbingRoutesModel, routesCmd ) =
            itemPageModel ClimbingRouteItem

        ( sectorsModel, sectorsCmd ) =
            itemPageModel SectorItem

        ( ascentsModel, ascentsCmd ) =
            itemPageModel AscentItem

        ( areasModel, areasCmd ) =
            itemPageModel AreaItem

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
                    Model.NotReady
      , url = url
      , route = parseUrl url
      , key = key
      , climbingRoutes = jsonFile.climbingRoutes
      , ascents = jsonFile.ascents
      , sectors = jsonFile.sectors
      , areas = jsonFile.areas
      , climbingRoutesModel = climbingRoutesModel
      , sectorsModel = sectorsModel
      , ascentsModel = ascentsModel
      , areasModel = areasModel
      , datePicker = datePicker
      }
    , Cmd.batch [ Cmd.map (ToDatePicker AscentItem "") datePickerCmd, routesCmd, sectorsCmd, areasCmd, ascentsCmd ]
    )


itemPageModel : Item -> ( ItemPageModel, Cmd Msg )
itemPageModel t =
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
    in
    ( { form = form
      , itemType = t
      , selectedItemId = Nothing
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
    { criteria =
        Dict.fromList
            [ ( "_parentId", { value = "", label = "_parentId", type_ = Model.Enumeration } )
            , ( "name", { value = "", label = "name", type_ = Model.String } )
            , ( "grade", { value = "", label = "grade", type_ = Model.String } )
            , ( "description", { value = "", label = "description", type_ = Model.String } )
            ]
    , order = [ "name", "grade", "description" ]
    , parentId = Nothing
    , formState = Hidden
    }


ascentForm : ItemPageItemForm
ascentForm =
    { criteria =
        Dict.fromList
            [ ( "_parentId", { value = "", label = "_parentId", type_ = Model.Enumeration } )
            , ( "date", { value = "", label = "date", type_ = Model.Date } )
            , ( "description", { value = "", label = "description", type_ = Model.String } )
            ]
    , order = [ "date", "description" ]
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
    { criteria =
        Dict.fromList
            [ ( "_parentId", { value = "", label = "_parentId", type_ = Model.Enumeration } )
            , ( "name", { value = "", label = "name", type_ = Model.String } )
            ]
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
    { criteria =
        Dict.fromList
            [ ( "name", { value = "", label = "name", type_ = Model.String } )
            , ( "country", { value = "", label = "country", type_ = Model.String } )
            ]
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
        , Parser.map RoutesRoute (Parser.s "routes")
        , Parser.map AscentsRoute (Parser.s "ascents")
        , Parser.map SectorsRoute (Parser.s "sectors")
        , Parser.map AreasRoute (Parser.s "areas")
        ]


parseUrl : Url -> Route
parseUrl url =
    url
        |> Parser.parse routeParser
        |> Maybe.withDefault NotFoundRoute
