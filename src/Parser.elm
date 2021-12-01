module Parser exposing (..)

import Data.Ascent exposing (Ascent)
import Data.ClimbingRoute exposing (ClimbingRoute)
import Dict exposing (Dict)
import Json.Decode exposing (int, list, string)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode


type alias Root =
    { routes : Dict Int ClimbingRoute
    , ascents : Dict Int Ascent
    }


rootDecoder : Json.Decode.Decoder Root
rootDecoder =
    let
        decodedRoutes : Json.Decode.Decoder (Dict Int ClimbingRoute)
        decodedRoutes =
            Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.field "id" int) rootRoutesObjectDecoder)
                |> Json.Decode.map Dict.fromList

        decodedAscents : Json.Decode.Decoder (Dict Int Ascent)
        decodedAscents =
            Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.field "id" int) ascentsDecoder)
                |> Json.Decode.map Dict.fromList
    in
    Json.Decode.map2 Root
        (Json.Decode.field "routes" <| decodedRoutes)
        (Json.Decode.field "ascents" <| decodedAscents)


rootRoutesObjectDecoder : Json.Decode.Decoder ClimbingRoute
rootRoutesObjectDecoder =
    Json.Decode.succeed ClimbingRoute
        |> required "id" int
        |> required "name" string
        |> required "grade" string
        |> optional "description" (Json.Decode.map Just string) Nothing
        |> optional "ascentIds" (Json.Decode.map Just (list int)) Nothing


ascentsDecoder : Json.Decode.Decoder Ascent
ascentsDecoder =
    Json.Decode.succeed Ascent
        |> required "id" int
        |> required "routeId" int
        |> required "date" string


encodedRoot : Root -> Json.Encode.Value
encodedRoot root =
    Json.Encode.object
        [ ( "routes", Json.Encode.list encodedClimbingRoute (Dict.values root.routes) )
        , ( "ascents", Json.Encode.list encodedAscent (Dict.values root.ascents) )
        ]


encodedClimbingRoute : ClimbingRoute -> Json.Encode.Value
encodedClimbingRoute route =
    Json.Encode.object
        [ ( "id", Json.Encode.int route.id )
        , ( "name", Json.Encode.string route.name )
        , ( "grade", Json.Encode.string route.grade )
        , ( "description", encodeNullable Json.Encode.string route.description )
        , ( "ascentIds", encodeNullable (Json.Encode.list Json.Encode.int) route.ascentIds )
        ]


encodedAscent : Ascent -> Json.Encode.Value
encodedAscent ascent =
    Json.Encode.object
        [ ( "id", Json.Encode.int ascent.id )
        , ( "routeId", Json.Encode.int ascent.routeId )
        , ( "date", Json.Encode.string ascent.date )
        ]


encodeNullable : (value -> Json.Encode.Value) -> Maybe value -> Json.Encode.Value
encodeNullable valueEncoder maybeValue =
    case maybeValue of
        Just value ->
            valueEncoder value

        Nothing ->
            Json.Encode.null
