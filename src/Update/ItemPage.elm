module Update.ItemPage exposing (..)

import Init exposing (initItemPageItemForm)
import Message exposing (Item(..), ItemPageMsg(..), Msg)
import Model exposing (ItemPageModel, Model)


getModelFromItem : Item -> Model -> ItemPageModel
getModelFromItem item model =
    case item of
        ClimbingRouteItem ->
            model.climbingRoutesModel

        SectorItem ->
            model.sectorsModel

        AscentItem ->
            model.ascentsModel


setItemPageModel : Item -> ItemPageModel -> Model -> Model
setItemPageModel item itemPageModel model =
    case item of
        ClimbingRouteItem ->
            { model | climbingRoutesModel = itemPageModel }

        SectorItem ->
            { model | sectorsModel = itemPageModel }

        AscentItem ->
            { model | ascentsModel = itemPageModel }


update : ItemPageMsg -> Item -> Model -> ( Model, Cmd Msg )
update msg item model =
    let
        itemPageModel =
            getModelFromItem item model

        updatedItemPageModel =
            case msg of
                OpenForm ->
                    { itemPageModel | form = Just initItemPageItemForm }

                CloseForm ->
                    { itemPageModel | form = Nothing }

                SelectItem id ->
                    { itemPageModel | selectedItemId = Just id }
    in
    ( setItemPageModel item updatedItemPageModel model, Cmd.none )
