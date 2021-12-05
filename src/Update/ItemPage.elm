module Update.ItemPage exposing (..)

import Dict exposing (Dict)
import Init exposing (areaForm, ascentForm, climbingRouteForm, sectorForm)
import Message exposing (CriteriumUpdate(..), Item(..), ItemPageMsg(..), Msg)
import Model exposing (Criterium, ItemPageItemForm, ItemPageModel, Model)
import Utilities.ItemPageUtilities exposing (getCriteriaFromItem, getModelFromItem)


setItemPageModel : Item -> ItemPageModel -> Model -> Model
setItemPageModel item itemPageModel model =
    case item of
        ClimbingRouteItem ->
            { model | climbingRoutesModel = itemPageModel }

        SectorItem ->
            { model | sectorsModel = itemPageModel }

        AscentItem ->
            { model | ascentsModel = itemPageModel }

        AreaItem ->
            { model | areasModel = itemPageModel }


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

                FormUpdateMessage criteriumUpdateMsg ->
                    let
                        maybeForm =
                            itemPageModel.form
                    in
                    case maybeForm of
                        Nothing ->
                            itemPageModel

                        Just form ->
                            let
                                updatedForm =
                                    case criteriumUpdateMsg of
                                        UpdateKey key value ->
                                            let
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
                                            in
                                            { form | criteria = updatedCriteria form.criteria }

                                        UpdateParent value ->
                                            { form | parentId = Just value }
                            in
                            { itemPageModel | form = Just updatedForm }
    in
    ( setItemPageModel item updatedItemPageModel model, Cmd.none )
