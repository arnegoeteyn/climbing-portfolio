module Update.Overview exposing (..)

import Json.Decode exposing (maybe)
import Message exposing (CriteriumUpdate(..), HomeMsg(..), ItemPageMsg(..), ItemType(..), Msg, OverviewMsg(..))
import Model exposing (Model)
import Select
import Utilities exposing (addIfNotPresent)
import View.Page.OverviewPage exposing (sectorSelectConfig)


update : Message.OverviewMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        overviewModel =
            model.overviewModel

        ( updatedModel, updatedCmd ) =
            case msg of
                RouteFilter filter ->
                    ( { overviewModel | routeFilter = filter }, Cmd.none )

                SelectMsg subMsg ->
                    let
                        ( updated, cmd ) =
                            Select.update sectorSelectConfig subMsg model.overviewModel.selectState
                    in
                    ( { overviewModel | selectState = updated }, cmd )

                OnSelect maybeItem ->
                    let
                        newSet =
                            case maybeItem of
                                Nothing ->
                                    overviewModel.selected

                                Just sector ->
                                    addIfNotPresent sector overviewModel.selected
                    in
                    ( { overviewModel | selected = newSet }, Cmd.none )

                OnRemove sector ->
                    let
                        newSet =
                            List.filter (\c -> c /= sector) overviewModel.selected
                    in
                    ( { overviewModel | selected = newSet }, Cmd.none )

                OnClimbingRouteClicked maybeClimbingRoute ->
                    ( { overviewModel | selectedClimbingRoute = maybeClimbingRoute }, Cmd.none )
    in
    ( { model | overviewModel = updatedModel }, updatedCmd )
