module Update.Home exposing (..)

import Message exposing (CriteriumUpdate(..), HomeMsg(..), ItemPageMsg(..), ItemType(..), Msg)
import Model exposing (Model)


update : Message.HomeMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        homeModel =
            model.homeModel

        updatedModel =
            case msg of
                OnHover hovering ->
                    { homeModel | hovering = hovering }
    in
    ( { model | homeModel = updatedModel }, Cmd.none )
