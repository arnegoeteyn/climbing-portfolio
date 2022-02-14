module Update.ItemPage exposing (..)

import Dict exposing (Dict)
import Message exposing (CriteriumUpdate(..), ItemPageMsg(..), ItemType(..), Msg, Route(..))
import Model exposing (Criterium, Model)
import Utilities.ItemFormUtilities as ItemFormUtilities
import Utilities.ItemPageUtilities as ItemPageUtilities


update : ItemPageMsg -> ItemType -> Model -> ( Model, Cmd Msg )
update msg item model =
    let
        itemPageModel =
            ItemPageUtilities.getModelFromItem item model

        ( updatedItemPageModel, updatedCmd ) =
            case msg of
                CreateNewItem ->
                    ( { itemPageModel | form = (\f -> { f | formState = Model.Create }) itemPageModel.form }, Cmd.none )

                UpdateItem itemId ->
                    let
                        criteria =
                            ItemFormUtilities.getCriteriaFromItem itemId itemPageModel.itemType model
                    in
                    ( { itemPageModel | form = (\f -> { f | formState = Model.Update itemId, criteria = criteria }) itemPageModel.form }, Cmd.none )

                CloseForm ->
                    ( { itemPageModel | form = (\f -> { f | formState = Model.Hidden }) itemPageModel.form }, Cmd.none )

                SelectItem id ->
                    ( { itemPageModel | selectedItemId = Just id, form = ItemFormUtilities.closeForm itemPageModel.form }, Cmd.none )

                FilterUpdateMessage key value ->
                    ( { itemPageModel | filters = Dict.insert key value itemPageModel.filters }, Cmd.none )

                FormUpdateMessage criteriumUpdateMsg ->
                    let
                        form =
                            itemPageModel.form

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
                    ( { itemPageModel | form = updatedForm }, Cmd.none )
    in
    ( ItemPageUtilities.setItemPageModel updatedItemPageModel model, updatedCmd )
