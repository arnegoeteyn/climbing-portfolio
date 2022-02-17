module View.Widget.EntityForm exposing (..)

import Date
import DatePicker
import Dict
import Html
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Message exposing (Msg)
import Model exposing (ItemPageModel, Model)
import Tailwind.Utilities as Tw
import Utilities
import Utilities.EntityPageUtilities as ItemPageUtilities
import Utilities.EntityUtilities as EntityUtilities


view : ItemPageModel -> Model -> Html Msg
view itemPageModel model =
    let
        viewCriterium criteria key =
            let
                maybeCriterium =
                    Dict.get key criteria
            in
            case maybeCriterium of
                Nothing ->
                    H.div [] []

                Just criterium ->
                    case criterium.type_ of
                        Model.String ->
                            Utilities.viewInput "text" criterium.label criterium.value (\value -> Message.ItemPage itemPageModel.itemType (Message.FormUpdateMessage <| Message.UpdateKey key value))

                        Model.Enumeration options ->
                            H.select
                                [ E.onInput (\value -> Message.ItemPage itemPageModel.itemType (Message.FormUpdateMessage <| Message.UpdateKey key value))
                                ]
                            <|
                                (options
                                    |> List.map
                                        (\item ->
                                            H.option
                                                [ A.value item
                                                , A.selected <| item == (Maybe.withDefault "" <| Maybe.map .value <| Dict.get key itemPageModel.form.criteria)
                                                ]
                                                [ H.text item ]
                                        )
                                )

                        Model.Date ->
                            DatePicker.view
                                (if String.isEmpty criterium.value then
                                    Nothing

                                 else
                                    Date.fromIsoString criterium.value |> Result.toMaybe
                                )
                                DatePicker.defaultSettings
                                model.datePicker
                                |> Html.map (Message.ToDatePicker itemPageModel.itemType key)
                                |> H.fromUnstyled

        maybeParentCriterium =
            EntityUtilities.getParentType itemPageModel.itemType
                |> Maybe.map
                    (\parentItem ->
                        H.select
                            [ E.onInput (\value -> Message.ItemPage itemPageModel.itemType (Message.FormUpdateMessage <| Message.UpdateParent value))
                            ]
                        <|
                            H.option [ A.value "" ] [ H.text "" ]
                                :: (ItemPageUtilities.sortedItems (ItemPageUtilities.getModelFromItem parentItem model) model
                                        |> List.map
                                            (\item ->
                                                H.option
                                                    [ A.value <| String.fromInt item.id
                                                    , A.selected <| String.fromInt item.id == (Maybe.withDefault "" <| Maybe.map .value <| Dict.get "_parentId" itemPageModel.form.criteria)
                                                    ]
                                                    [ H.text item.identifier ]
                                            )
                                   )
                    )
    in
    H.div []
        [ H.div [ A.css [ Tw.flex, Tw.flex_col ] ] <| List.map (viewCriterium itemPageModel.form.criteria) itemPageModel.form.order
        , H.div [] [ Maybe.withDefault (H.text "") maybeParentCriterium ]
        ]
