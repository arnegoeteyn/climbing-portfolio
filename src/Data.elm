module Data exposing (Ascent, AscentKind, ClimbingRoute, encodedJsonFile, jsonFileDecoder)

import Dict exposing (Dict)
import Json.Decode exposing (int, list, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode
import Utilities exposing (encodeNullable)


type alias JsonFile =
    { climbingRoutes : Dict Int ClimbingRoute
    , ascents : Dict Int Ascent
    }


jsonFileDecoder : Json.Decode.Decoder JsonFile
jsonFileDecoder =
    let
        decodedRoutes : Json.Decode.Decoder (Dict Int ClimbingRoute)
        decodedRoutes =
            Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.field "id" int) climbingRouteDecoder)
                |> Json.Decode.map Dict.fromList

        decodedAscents : Json.Decode.Decoder (Dict Int Ascent)
        decodedAscents =
            Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.field "id" int) ascentsDecoder)
                |> Json.Decode.map Dict.fromList
    in
    Json.Decode.map2 JsonFile
        (Json.Decode.field "routes" <| decodedRoutes)
        (Json.Decode.field "ascents" <| decodedAscents)


encodedJsonFile : JsonFile -> Json.Encode.Value
encodedJsonFile root =
    Json.Encode.object
        [ ( "routes", Json.Encode.list encodeClimbingRoute (Dict.values root.climbingRoutes) )
        , ( "ascents", Json.Encode.list encodeAscent (Dict.values root.ascents) )
        ]


type alias ClimbingRoute =
    { id : Int
    , name : String
    , grade : String
    , description : Maybe String
    , ascentIds : Maybe (List Int)
    }


climbingRouteDecoder : Json.Decode.Decoder ClimbingRoute
climbingRouteDecoder =
    Json.Decode.succeed ClimbingRoute
        |> required "id" int
        |> required "name" string
        |> required "grade" string
        |> optional "description" (Json.Decode.map Just string) Nothing
        |> optional "ascentIds" (Json.Decode.map Just (list int)) Nothing


encodeClimbingRoute : ClimbingRoute -> Json.Encode.Value
encodeClimbingRoute route =
    Json.Encode.object
        [ ( "id", Json.Encode.int route.id )
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
