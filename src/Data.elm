module Data exposing (..)

import Dict exposing (Dict)
import Json.Decode exposing (int, list, string)
import Json.Decode.Extra exposing (set)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode exposing (encode)
import Set exposing (Set)
import Svg.Styled.Attributes exposing (ascent)
import Utilities exposing (encodeNullable)


type alias ItemPageItem =
    { cardHeader : String
    , identifier : String
    , cardDescription : Maybe String
    , id : Int
    , parentId : Maybe Int
    , childIds : Maybe (Set Int)
    }


type alias JsonFile =
    { climbingRoutes : Dict Int ClimbingRoute
    , ascents : Dict Int Ascent
    , sectors : Dict Int Sector
    , areas : Dict Int Area
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

        decodedAreas =
            generalDecoder areaDecoder
    in
    Json.Decode.map4 JsonFile
        (Json.Decode.field "routes" <| decodedRoutes)
        (Json.Decode.field "ascents" <| decodedAscents)
        (Json.Decode.field "sectors" <| decodedSectors)
        (Json.Decode.field "areas" <| decodedAreas)


encodedJsonFile : JsonFile -> Json.Encode.Value
encodedJsonFile root =
    Json.Encode.object
        [ ( "routes", Json.Encode.list encodeClimbingRoute (Dict.values root.climbingRoutes) )
        , ( "ascents", Json.Encode.list encodeAscent (Dict.values root.ascents) )
        , ( "sectors", Json.Encode.list encodeSector (Dict.values root.sectors) )
        , ( "areas", Json.Encode.list encodeArea (Dict.values root.areas) )
        ]


type ClimbingRouteKind
    = Boulder
    | Sport


climbingRouteKindDecoder : Json.Decode.Decoder ClimbingRouteKind
climbingRouteKindDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case String.toLower str of
                    "sport" ->
                        Json.Decode.succeed Sport

                    "boulder" ->
                        Json.Decode.succeed Boulder

                    _ ->
                        Json.Decode.fail "invalid routeKind"
            )


climbingRouteKindToString kind =
    case kind of
        Sport ->
            "sport"

        Boulder ->
            "boulder"


encodeClimbingRouteKind : ClimbingRouteKind -> Json.Encode.Value
encodeClimbingRouteKind =
    Json.Encode.string << climbingRouteKindToString


type alias ClimbingRoute =
    { id : Int
    , sectorId : Maybe Int
    , name : String
    , grade : String
    , description : Maybe String
    , ascentIds : Maybe (Set Int)
    , kind : ClimbingRouteKind
    }


climbingRouteDecoder : Json.Decode.Decoder ClimbingRoute
climbingRouteDecoder =
    Json.Decode.succeed ClimbingRoute
        |> required "id" int
        |> optional "sectorId" (Json.Decode.map Just int) Nothing
        |> required "name" string
        |> required "grade" string
        |> optional "description" (Json.Decode.map Just string) Nothing
        |> optional "ascentIds" (Json.Decode.map Just (set int)) Nothing
        |> required "kind" climbingRouteKindDecoder


encodeClimbingRoute : ClimbingRoute -> Json.Encode.Value
encodeClimbingRoute route =
    Json.Encode.object
        [ ( "id", Json.Encode.int route.id )
        , ( "sectorId", encodeNullable Json.Encode.int route.sectorId )
        , ( "name", Json.Encode.string route.name )
        , ( "grade", Json.Encode.string route.grade )
        , ( "description", encodeNullable Json.Encode.string route.description )
        , ( "ascentIds", encodeNullable (Json.Encode.set Json.Encode.int) route.ascentIds )
        , ( "kind", encodeClimbingRouteKind route.kind )
        ]


type alias Ascent =
    { id : Int
    , routeId : Maybe Int
    , date : Maybe String
    , description : Maybe String
    , kind : AscentKind
    }


type AscentKind
    = Onsight
    | Flash
    | SecondGo
    | Redpoint
    | Repeat


ascentKindDecoder : Json.Decode.Decoder AscentKind
ascentKindDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case String.toLower str of
                    "onsight" ->
                        Json.Decode.succeed Onsight

                    "redpoint" ->
                        Json.Decode.succeed Redpoint

                    "flash" ->
                        Json.Decode.succeed Flash

                    "repeat" ->
                        Json.Decode.succeed Repeat

                    "secondgo" ->
                        Json.Decode.succeed SecondGo

                    _ ->
                        Json.Decode.fail "invalid routeKind"
            )


encodeAscentKind : AscentKind -> Json.Encode.Value
encodeAscentKind =
    Json.Encode.string << ascentKindToString


ascentKindToString kind =
    case kind of
        Redpoint ->
            "redpoint"

        Flash ->
            "flash"

        Onsight ->
            "onsight"

        SecondGo ->
            "secondgo"

        Repeat ->
            "repeat"


ascentsDecoder : Json.Decode.Decoder Ascent
ascentsDecoder =
    Json.Decode.succeed Ascent
        |> required "id" int
        |> optional "routeId" (Json.Decode.map Just int) Nothing
        |> optional "date" (Json.Decode.map Just string) Nothing
        |> optional "description" (Json.Decode.map Just string) Nothing
        |> required "kind" ascentKindDecoder


encodeAscent : Ascent -> Json.Encode.Value
encodeAscent ascent =
    Json.Encode.object
        [ ( "id", Json.Encode.int ascent.id )
        , ( "routeId", encodeNullable Json.Encode.int ascent.routeId )
        , ( "description", encodeNullable Json.Encode.string ascent.description )
        , ( "date", encodeNullable Json.Encode.string ascent.date )
        , ( "kind", encodeAscentKind ascent.kind )
        ]


type alias Sector =
    { id : Int
    , areaId : Maybe Int
    , name : String
    , routeIds : Maybe (Set Int)
    }


sectorDecoder : Json.Decode.Decoder Sector
sectorDecoder =
    Json.Decode.succeed Sector
        |> required "id" int
        |> optional "areaId" (Json.Decode.map Just int) Nothing
        |> required "name" string
        |> optional "routeIds" (Json.Decode.map Just (set int)) Nothing


encodeSector : Sector -> Json.Encode.Value
encodeSector sector =
    Json.Encode.object
        [ ( "id", Json.Encode.int sector.id )
        , ( "name", Json.Encode.string sector.name )
        , ( "routeIds", encodeNullable (Json.Encode.set Json.Encode.int) sector.routeIds )
        , ( "areaId", encodeNullable Json.Encode.int sector.areaId )
        ]


type alias Area =
    { id : Int
    , name : String
    , country : String
    , sectorIds : Maybe (Set Int)
    }


areaDecoder : Json.Decode.Decoder Area
areaDecoder =
    Json.Decode.succeed Area
        |> required "id" int
        |> required "name" string
        |> required "country" string
        |> optional "sectorIds" (Json.Decode.map Just (set int)) Nothing


encodeArea : Area -> Json.Encode.Value
encodeArea area =
    Json.Encode.object
        [ ( "id", Json.Encode.int area.id )
        , ( "name", Json.Encode.string area.name )
        , ( "country", Json.Encode.string area.country )
        , ( "sectorIds", encodeNullable (Json.Encode.set Json.Encode.int) area.sectorIds )
        ]
