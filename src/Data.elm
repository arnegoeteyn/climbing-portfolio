module Data exposing (..)

import Dict exposing (Dict)
import Json.Decode exposing (int, list, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode
import Utilities exposing (encodeNullable)


type alias ItemPageItem =
    { cardHeader : String
    , identifier : String
    , id : Int
    }


type alias JsonFile =
    { climbingRoutes : Dict Int ClimbingRoute
    , ascents : Dict Int Ascent
    , sectors : Dict Int Sector
    }


jsonFileDecoder : Json.Decode.Decoder JsonFile
jsonFileDecoder =
    let
        generalDecoder specificDecoder =
            Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.field "id" int) specificDecoder)
                |> Json.Decode.map Dict.fromList

        decodedRoutes =
            generalDecoder climbingRouteDecoder

        decodedAscents =
            generalDecoder ascentsDecoder

        decodedSectors =
            generalDecoder sectorDecoder
    in
    Json.Decode.map3 JsonFile
        (Json.Decode.field "routes" <| decodedRoutes)
        (Json.Decode.field "ascents" <| decodedAscents)
        (Json.Decode.field "sectors" <| decodedSectors)


encodedJsonFile : JsonFile -> Json.Encode.Value
encodedJsonFile root =
    Json.Encode.object
        [ ( "routes", Json.Encode.list encodeClimbingRoute (Dict.values root.climbingRoutes) )
        , ( "ascents", Json.Encode.list encodeAscent (Dict.values root.ascents) )
        , ( "sectors", Json.Encode.list encodeSector (Dict.values root.sectors) )
        ]


type alias ClimbingRoute =
    { id : Int
    , sectorId : Maybe Int
    , name : String
    , grade : String
    , description : Maybe String
    , ascentIds : Maybe (List Int)
    }


climbingRouteDecoder : Json.Decode.Decoder ClimbingRoute
climbingRouteDecoder =
    Json.Decode.succeed ClimbingRoute
        |> required "id" int
        |> optional "sectorId" (Json.Decode.map Just int) Nothing
        |> required "name" string
        |> required "grade" string
        |> optional "description" (Json.Decode.map Just string) Nothing
        |> optional "ascentIds" (Json.Decode.map Just (list int)) Nothing


encodeClimbingRoute : ClimbingRoute -> Json.Encode.Value
encodeClimbingRoute route =
    Json.Encode.object
        [ ( "id", Json.Encode.int route.id )
        , ( "sectorId", encodeNullable Json.Encode.int route.sectorId )
        , ( "name", Json.Encode.string route.name )
        , ( "grade", Json.Encode.string route.grade )
        , ( "description", encodeNullable Json.Encode.string route.description )
        , ( "ascentIds", encodeNullable (Json.Encode.list Json.Encode.int) route.ascentIds )
        ]


type alias Ascent =
    { id : Int
    , routeId : Int
    , date : String
    }


type AscentKind
    = Onsight
    | Flash
    | SecondGo
    | Redpoint
    | Repeat


ascentsDecoder : Json.Decode.Decoder Ascent
ascentsDecoder =
    Json.Decode.succeed Ascent
        |> required "id" int
        |> required "routeId" int
        |> required "date" string


encodeAscent : Ascent -> Json.Encode.Value
encodeAscent ascent =
    Json.Encode.object
        [ ( "id", Json.Encode.int ascent.id )
        , ( "routeId", Json.Encode.int ascent.routeId )
        , ( "date", Json.Encode.string ascent.date )
        ]


type alias Sector =
    { id : Int
    , name : String
    , routeIds : Maybe (List Int)
    }


sectorDecoder : Json.Decode.Decoder Sector
sectorDecoder =
    Json.Decode.succeed Sector
        |> required "id" int
        |> required "name" string
        |> optional "routeIds" (Json.Decode.map Just (list int)) Nothing


encodeSector : Sector -> Json.Encode.Value
encodeSector sector =
    Json.Encode.object
        [ ( "id", Json.Encode.int sector.id )
        , ( "name", Json.Encode.string sector.name )
        , ( "routeIds", encodeNullable (Json.Encode.list Json.Encode.int) sector.routeIds )
        ]
