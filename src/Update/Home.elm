module Update.Home exposing (..)

import Dict exposing (Dict)
import Message exposing (CriteriumUpdate(..), HomeMsg(..), ItemPageMsg(..), ItemType(..), Msg)
import Model exposing (Criterium, Model)


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
