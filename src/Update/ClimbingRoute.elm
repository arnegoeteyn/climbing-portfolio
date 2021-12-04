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

                Message.FormName name ->
                    ( climbingRouteModel, Cmd.none )

                -- let
                --     formUpdate form =
                --         Just { form | name = name }
                --     newForm =
                --         Maybe.andThen formUpdate climbingRouteModel.form
                -- in
                -- ( { climbingRouteModel | form = newForm }, Cmd.none )
                Message.FormGrade grade ->
                    ( climbingRouteModel, Cmd.none )

                -- let
                --     newForm =
                --         Maybe.andThen (\form -> Just { form | grade = grade }) climbingRouteModel.form
                -- in
                -- ( { climbingRouteModel | form = newForm }, Cmd.none )
                Message.FormSector sectorIdString ->
                    ( climbingRouteModel, Cmd.none )

        -- case climbingRouteModel.form of
        --     Just form ->
        --         ( { form | sectorId = sectorIdString }
        --             |> (\newForm -> { climbingRouteModel | form = Just newForm })
        --         , Cmd.none
        --         )
        --     Nothing ->
        --         ( climbingRouteModel, Cmd.none )
    in
    ( { model | climbingRoutesModel = updatedClimbingRouteModel }, cmdMsg )
