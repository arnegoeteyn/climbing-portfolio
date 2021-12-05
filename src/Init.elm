module Init exposing (..)

import Browser.Navigation exposing (Key)
import Data exposing (ClimbingRoute, jsonFileDecoder)
import DatePicker
import Dict exposing (Dict)
import Json.Decode exposing (decodeString)
import Message exposing (ClimbingRouteMsg(..), Item(..), ItemRelation, Msg, Route(..))
import Model exposing (ClimbingRoutesModel, Criterium, ItemPageItemForm, ItemPageModel, Model, SectorsModel)
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
      , sectorsModel = sectorsModel
      , ascentsModel = ascentsModel
      }
    , Cmd.batch [ routesCmd, sectorsCmd ]
    )


itemPageModel : Item -> ( ItemPageModel, Cmd Msg )
itemPageModel t =
    ( { form = Nothing
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
            [ ( "_parentId", { value = "", label = "_parentId" } )
            , ( "name", { value = "", label = "name" } )
            , ( "grade", { value = "", label = "grade" } )
            , ( "description", { value = "", label = "description" } )
            ]
    , order = [ "name", "grade", "description" ]
    , parentId = Nothing
    }


ascentForm : ItemPageItemForm
ascentForm =
    { criteria = Dict.empty
    , order = []
    , parentId = Nothing
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
            [ ( "name", { value = "", label = "name" } )
            ]
    , order = [ "name" ]
    , parentId = Nothing
    }


sectorRelations : ItemRelation
sectorRelations =
    { parent = Nothing
    , child = Just ClimbingRouteItem
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
