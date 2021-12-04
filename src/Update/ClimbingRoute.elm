module Update.ClimbingRoute exposing (..)

import Message exposing (ClimbingRouteMsg, Msg)
import Model exposing (Model)
import Utilities exposing (newId)


updateClimbingRoute : ClimbingRouteMsg -> Model -> ( Model, Cmd Msg )
updateClimbingRoute msg model =
    let
        climbingRouteModel =
            model.climbingRoutesModel

        ( updatedClimbingRouteModel, cmdMsg ) =
            case msg of
                Message.AddAscentButtonClicked ->
                    ( climbingRouteModel, Cmd.none )
    in
    ( { model | climbingRoutesModel = updatedClimbingRouteModel }, cmdMsg )
