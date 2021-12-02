module Update.ClimbingRoute exposing (..)

import Date
import DatePicker
import Dict
import Init exposing (initClimbingRouteForm)
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
                Message.ClimbingRouteSelected route ->
                    ( { climbingRouteModel | selectedRoute = Just route }, Cmd.none )

                Message.AddAscentButtonClicked ->
                    ( { climbingRouteModel | showNewAscentDate = not climbingRouteModel.showNewAscentDate }, Cmd.none )

                Message.FormName name ->
                    let
                        formUpdate form =
                            Just { form | name = name }

                        newForm =
                            Maybe.andThen formUpdate climbingRouteModel.form
                    in
                    ( { climbingRouteModel | form = newForm }, Cmd.none )

                Message.FormGrade grade ->
                    let
                        newForm =
                            Maybe.andThen (\form -> Just { form | grade = grade }) climbingRouteModel.form
                    in
                    ( { climbingRouteModel | form = newForm }, Cmd.none )

                Message.ShowNewRouteForm ->
                    ( { climbingRouteModel | form = Just initClimbingRouteForm }, Cmd.none )

                Message.CloseNewRouteForm ->
                    ( { climbingRouteModel | form = Nothing }, Cmd.none )
    in
    ( { model | climbingRoutesModel = updatedClimbingRouteModel }, cmdMsg )
