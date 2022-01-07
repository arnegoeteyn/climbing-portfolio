module Update.ItemPage exposing (..)

import Dict exposing (Dict)
import Message exposing (CriteriumUpdate(..), Item(..), ItemPageMsg(..), Msg)
import Model exposing (Criterium, ItemPageModel, Model)
import Utilities.ItemPageUtilities as ItemPageUtilities


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
            ItemPageUtilities.getModelFromItem item model

        updatedItemPageModel =
            case msg of
                CreateNewItem ->
                    { itemPageModel | form = (\f -> { f | formState = Model.Create }) itemPageModel.form }

                UpdateItem itemId ->
                    let
                        criteria =
                            ItemPageUtilities.getCriteriaFromItem itemId itemPageModel.itemType model
                    in
                    { itemPageModel | form = (\f -> { f | formState = Model.Update itemId, criteria = criteria }) itemPageModel.form }

                CloseForm ->
                    { itemPageModel | form = (\f -> { f | formState = Model.Hidden }) itemPageModel.form }

                SelectItem id ->
                    { itemPageModel | selectedItemId = Just id }

                FilterUpdateMessage key value ->
                    { itemPageModel | filters = Dict.insert key value itemPageModel.filters }

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
                    { itemPageModel | form = updatedForm }
    in
    ( setItemPageModel item updatedItemPageModel model, Cmd.none )
