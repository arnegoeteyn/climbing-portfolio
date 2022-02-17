module View.Widget.EntityForm exposing (..)

import Data
import Date
import DatePicker
import Dict
import Html
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as E
import Message exposing (ItemType(..), Msg)
import Model exposing (EntityForm, Model)
import Tailwind.Utilities as Tw
import Utilities
import Utilities.EntityPageUtilities as ItemPageUtilities
import Utilities.EntityUtilities as EntityUtilities


view : EntityForm -> Model -> Html Msg
view form model =
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
                            Utilities.viewInput "text" criterium.label criterium.value (\value -> Message.ItemPage form.entity.itemType (Message.FormUpdateMessage <| Message.UpdateKey key value))

                        Model.Enumeration options ->
                            H.select
                                [ E.onInput (\value -> Message.ItemPage form.entity.itemType (Message.FormUpdateMessage <| Message.UpdateKey key value))
                                ]
                            <|
                                (options
                                    |> List.map
                                        (\item ->
                                            H.option
                                                [ A.value item
                                                , A.selected <| item == (Maybe.withDefault "" <| Maybe.map .value <| Dict.get key form.criteria)
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
                                |> Html.map (Message.ToDatePicker form.entity.itemType key)
                                |> H.fromUnstyled

        maybeParentCriterium =
            EntityUtilities.getParentType form.entity.itemType
                |> Debug.log "parent"
                |> Maybe.map
                    (\parentType ->
                        H.select
                            [ E.onInput (\value -> Message.ItemPage form.entity.itemType (Message.FormUpdateMessage <| Message.UpdateParent value))
                            ]
                        <|
                            H.option [ A.value "" ] [ H.text "" ]
                                :: (ItemPageUtilities.sortedItems parentType model
                                        |> List.map
                                            (\id -> viewItemSelector form id model)
                                   )
                    )
    in
    H.div []
        [ H.div [ A.css [ Tw.flex, Tw.flex_col ] ] <| List.map (viewCriterium form.criteria) form.order
        , H.div [] [ Maybe.withDefault (H.text "") maybeParentCriterium ]
        ]


viewItemSelector : EntityForm -> Int -> Model -> Html Msg
viewItemSelector form id model =
    H.option
        [ A.value <| String.fromInt id
        , A.selected <| String.fromInt id == (Maybe.withDefault "" <| Maybe.map .value <| Dict.get "_parentId" form.criteria)
        ]
        [ H.text <|
            Maybe.withDefault "" <|
                case form.entity.itemType of
                    AreaItem ->
                        Nothing

                    SectorItem ->
                        EntityUtilities.getArea id model |> Maybe.map .name

                    ClimbingRouteItem ->
                        EntityUtilities.getSector id model |> Maybe.map .name

                    AscentItem ->
                        EntityUtilities.getClimbingRoute id model |> Maybe.map (\c -> Utilities.stringFromList [ c.name, " [", c.grade, "]" ])
        ]
