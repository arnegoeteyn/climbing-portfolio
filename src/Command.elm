port module Command exposing (..)

import Data exposing (JsonFile, encodedJsonFile)
import Json.Encode
import Message exposing (Msg)
import Model exposing (Model)
import Update exposing (update)



-- Side effects


port storeCache : Json.Encode.Value -> Cmd msg


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch
        [ storeCache
            (encodedJsonFile
                { climbingRoutes = newModel.climbingRoutes, ascents = newModel.ascents, sectors = newModel.sectors, areas = newModel.areas }
            )
        , cmds
        ]
    )
