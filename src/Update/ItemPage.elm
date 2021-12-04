module Update.ItemPage exposing (..)

import Dict exposing (Dict)
import Init exposing (ascentForm, climbingRouteForm, sectorForm)
import Message exposing (Item(..), ItemPageMsg(..), Msg)
import Model exposing (Criterium, ItemPageItemForm, ItemPageModel, Model)


getModelFromItem : Item -> Model -> ItemPageModel
getModelFromItem item model =
    case item of
        ClimbingRouteItem ->
            model.climbingRoutesModel

        SectorItem ->
            model.sectorsModel

        AscentItem ->
            model.ascentsModel


getCriteriaFromItem : Item -> ItemPageItemForm
getCriteriaFromItem item =
    case item of
        ClimbingRouteItem ->
            climbingRouteForm

        AscentItem ->
            ascentForm

        SectorItem ->
            sectorForm


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
                    { itemPageModel | form = Just <| getCriteriaFromItem item }

                CloseForm ->
                    { itemPageModel | form = Nothing }

                SelectItem id ->
                    { itemPageModel | selectedItemId = Just id }

                FormUpdateMessage key value ->
                    let
                        maybeForm =
                            itemPageModel.form

                        updatedFormCriterium : Dict String Criterium -> Maybe Criterium
                        updatedFormCriterium formCriteria =
                            let
                                formItem =
                                    Dict.get key formCriteria
                            in
                            Maybe.map (\aFormItem -> { aFormItem | value = value }) formItem

                        updatedCriteria formCriteria =
                            let
                                maybeItem =
                                    updatedFormCriterium formCriteria
                            in
                            case maybeItem of
                                Nothing ->
                                    formCriteria

                                Just i ->
                                    Dict.insert key i formCriteria

                        updatedForm form =
                            { form | criteria = updatedCriteria form.criteria }
                    in
                    case maybeForm of
                        Nothing ->
                            itemPageModel

                        Just form ->
                            { itemPageModel | form = Just <| updatedForm form }
    in
    ( setItemPageModel item updatedItemPageModel model, Cmd.none )
