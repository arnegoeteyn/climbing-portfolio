module Update.ItemPage exposing (..)

import Browser.Navigation exposing (pushUrl)
import Dict exposing (Dict)
import Init exposing (parseUrl)
import Message exposing (CriteriumUpdate(..), Item(..), ItemPageMsg(..), Msg, Route(..))
import Model exposing (Criterium, ItemPageModel, Model)
import Url
import Url.Builder
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

        ( updatedItemPageModel, updatedCmd ) =
            case msg of
                CreateNewItem ->
                    ( { itemPageModel | form = (\f -> { f | formState = Model.Create }) itemPageModel.form }, Cmd.none )

                UpdateItem itemId ->
                    let
                        criteria =
                            ItemPageUtilities.getCriteriaFromItem itemId itemPageModel.itemType model
                    in
                    ( { itemPageModel | form = (\f -> { f | formState = Model.Update itemId, criteria = criteria }) itemPageModel.form }, Cmd.none )

                CloseForm ->
                    ( { itemPageModel | form = (\f -> { f | formState = Model.Hidden }) itemPageModel.form }, Cmd.none )

                SelectItem id ->
                    let
                        newUrl =
                            ItemPageUtilities.urlToItem itemPageModel.itemType id
                    in
                    ( { itemPageModel | selectedItemId = Just id }, Cmd.none )

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
    ( setItemPageModel item updatedItemPageModel model, updatedCmd )
