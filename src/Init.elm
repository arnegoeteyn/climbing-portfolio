module Init exposing (..)

import Browser.Navigation exposing (Key)
import Data exposing (ClimbingRoute, Sector, jsonFileDecoder)
import Dict exposing (Dict)
import Json.Decode exposing (decodeString)
import Message exposing (ClimbingRouteMsg(..), Item(..), ItemRelation, Msg, Route(..))
import Model exposing (ClimbingRoutesModel, Criterium, FormState(..), ItemPageItemForm, ItemPageModel, Model, SectorsModel)
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
      }
    , Cmd.batch [ routesCmd, sectorsCmd, areasCmd, ascentsCmd ]
    )


itemPageModel : Item -> ( ItemPageModel, Cmd Msg )
itemPageModel t =
    ( { form = climbingRouteForm
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
    , formState = Hidden
    }


ascentForm : ItemPageItemForm
ascentForm =
    { criteria = Dict.empty
    , order = []
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
            [ ( "name", { value = "", label = "name" } )
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
            [ ( "name", { value = "", label = "name" } )
            , ( "country", { value = "", label = "name" } )
            ]
    , order = [ "name" ]
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
