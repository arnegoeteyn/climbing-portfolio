module Data exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)
import Json.Decode exposing (fail, int, string, succeed)
import Json.Decode.Extra exposing (set)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode
import Set exposing (Set)
import Svg.Styled.Attributes exposing (ascent)
import Utilities exposing (encodeNullable)


type alias JsonFile =
    { climbingRoutes : Dict Int ClimbingRoute
    , ascents : Dict Int Ascent
    , sectors : Dict Int Sector
    , areas : Dict Int Area
    , trips : Dict Int Trip
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

        decodedTrips =
            generalDecoder tripDecoder
    in
    Json.Decode.map5 JsonFile
        (Json.Decode.field "routes" <| decodedRoutes)
        (Json.Decode.field "ascents" <| decodedAscents)
        (Json.Decode.field "sectors" <| decodedSectors)
        (Json.Decode.field "areas" <| decodedAreas)
        (Json.Decode.field "trips" <| decodedTrips)


encodedJsonFile : JsonFile -> Json.Encode.Value
encodedJsonFile root =
    Json.Encode.object
        [ ( "routes", Json.Encode.list encodeClimbingRoute (Dict.values root.climbingRoutes) )
        , ( "ascents", Json.Encode.list encodeAscent (Dict.values root.ascents) )
        , ( "sectors", Json.Encode.list encodeSector (Dict.values root.sectors) )
        , ( "areas", Json.Encode.list encodeArea (Dict.values root.areas) )
        , ( "trips", Json.Encode.list encodeTrip (Dict.values root.trips) )
        ]


type ClimbingRouteKind
    = Boulder
    | Sport


climbingRouteKindDecoder : Json.Decode.Decoder ClimbingRouteKind
climbingRouteKindDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case climbingRouteKindFromString str of
                    Just value ->
                        Json.Decode.succeed value

                    Nothing ->
                        Json.Decode.fail "invalid routeKind"
            )


climbingRouteKindFromString : String -> Maybe ClimbingRouteKind
climbingRouteKindFromString s =
    case String.toLower s of
        "sport" ->
            Just Sport

        "boulder" ->
            Just Boulder

        _ ->
            Nothing


climbingRouteKindToString : ClimbingRouteKind -> String
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
    , comment : Maybe String
    , ascentIds : Set Int
    , kind : ClimbingRouteKind
    , media : List String
    }


climbingRouteDecoder : Json.Decode.Decoder ClimbingRoute
climbingRouteDecoder =
    Json.Decode.succeed ClimbingRoute
        |> required "id" int
        |> optional "sectorId" (Json.Decode.map Just int) Nothing
        |> required "name" string
        |> required "grade" string
        |> optional "comment" (Json.Decode.map Just string) Nothing
        |> optional "ascentIds" (set int) Set.empty
        |> required "kind" climbingRouteKindDecoder
        |> optional "media" (Json.Decode.list Json.Decode.string) []


encodeClimbingRoute : ClimbingRoute -> Json.Encode.Value
encodeClimbingRoute route =
    Json.Encode.object
        [ ( "id", Json.Encode.int route.id )
        , ( "sectorId", encodeNullable Json.Encode.int route.sectorId )
        , ( "name", Json.Encode.string route.name )
        , ( "grade", Json.Encode.string route.grade )
        , ( "comment", encodeNullable Json.Encode.string route.comment )
        , ( "ascentIds", Json.Encode.set Json.Encode.int route.ascentIds )
        , ( "kind", encodeClimbingRouteKind route.kind )
        , ( "media", Json.Encode.list Json.Encode.string route.media )
        ]


type alias Ascent =
    { id : Int
    , routeId : Maybe Int
    , date : Maybe Date
    , comment : Maybe String
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
                case ascentKindFromString str of
                    Just value ->
                        Json.Decode.succeed value

                    _ ->
                        Json.Decode.fail "invalid routeKind"
            )


encodeAscentKind : AscentKind -> Json.Encode.Value
encodeAscentKind =
    Json.Encode.string << ascentKindToString


ascentKindFromString : String -> Maybe AscentKind
ascentKindFromString s =
    case String.toLower s of
        "onsight" ->
            Just Onsight

        "redpoint" ->
            Just Redpoint

        "flash" ->
            Just Flash

        "repeat" ->
            Just Repeat

        "secondgo" ->
            Just SecondGo

        _ ->
            Nothing


ascentKindToString : AscentKind -> String
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
        |> optional "date" (Json.Decode.map Just dateDecoder) Nothing
        |> optional "comment" (Json.Decode.map Just string) Nothing
        |> required "kind" ascentKindDecoder


encodeAscent : Ascent -> Json.Encode.Value
encodeAscent ascent =
    Json.Encode.object
        [ ( "id", Json.Encode.int ascent.id )
        , ( "routeId", encodeNullable Json.Encode.int ascent.routeId )
        , ( "comment", encodeNullable Json.Encode.string ascent.comment )
        , ( "date", encodeNullable Json.Encode.string <| Maybe.map Date.toIsoString ascent.date )
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


type alias CriteriumValue =
    { key : String
    , value : String
    }


criteriumValueDecoder : Json.Decode.Decoder CriteriumValue
criteriumValueDecoder =
    Json.Decode.succeed CriteriumValue
        |> required "key" string
        |> required "value" string


criteriumValueListDecoder : Json.Decode.Decoder (List CriteriumValue)
criteriumValueListDecoder =
    Json.Decode.list criteriumValueDecoder


encodeCriteriumValue : CriteriumValue -> Json.Encode.Value
encodeCriteriumValue criteriumValue =
    Json.Encode.object
        [ ( "key", Json.Encode.string criteriumValue.key )
        , ( "value", Json.Encode.string criteriumValue.value )
        ]


encodeCriteriumValueList : List CriteriumValue -> Json.Encode.Value
encodeCriteriumValueList l =
    Json.Encode.list encodeCriteriumValue l


type alias Trip =
    { id : Int
    , from : Date
    , to : Date
    }


tripDecoder : Json.Decode.Decoder Trip
tripDecoder =
    Json.Decode.succeed Trip
        |> required "id" int
        |> required "from" dateDecoder
        |> required "to" dateDecoder


encodeTrip : Trip -> Json.Encode.Value
encodeTrip trip =
    Json.Encode.object
        [ ( "id", Json.Encode.int trip.id )
        , ( "from", Json.Encode.string (Date.toIsoString trip.from) )
        , ( "to", Json.Encode.string (Date.toIsoString trip.to) )
        ]


dateDecoder : Json.Decode.Decoder Date
dateDecoder =
    string
        |> Json.Decode.andThen
            (\val ->
                case
                    Date.fromIsoString val
                of
                    Err x ->
                        fail x

                    Ok value ->
                        succeed value
            )
