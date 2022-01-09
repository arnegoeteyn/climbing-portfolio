module View.Page.Home exposing (viewHome)

import Array
import Chart as C
import Chart.Attributes as CA
import Chart.Item as CI
import Data exposing (ClimbingRouteKind)
import Dict
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Message exposing (Msg)
import Model exposing (Model)
import Set
import Svg as S
import Tailwind.Utilities as Tw


viewHome : Model -> Html Msg
viewHome model =
    H.div [ A.css [ Tw.grid ] ]
        [ routesPerGrade Data.Sport model
        , routesPerGrade Data.Boulder model
        ]


routesPerGrade : ClimbingRouteKind -> Model -> Html Msg
routesPerGrade kind model =
    let
        grades =
            model.climbingRoutes
                |> Dict.filter (\key value -> value.kind == kind)
                |> Dict.filter (\key value -> not << Set.isEmpty << Maybe.withDefault Set.empty <| value.ascentIds)
                |> Dict.map (\key item -> item.grade)
                |> Dict.foldl
                    (\key value acc ->
                        Dict.insert value
                            (Dict.get value acc |> Maybe.withDefault 0 |> (+) 1)
                            acc
                    )
                    Dict.empty
                |> Debug.log "grades"

        gradesValues =
            grades
                |> Dict.keys
                |> Array.fromList
    in
    H.div [ A.css [ Tw.p_36 ] ] <|
        [ H.fromUnstyled <|
            C.chart
                [ CA.height 300, CA.width 600 ]
                [ C.eachBin <| \p bin -> [ C.label [ CA.moveDown 18 ] [ S.text <| Maybe.withDefault "" <| Array.get ((\x -> x - 1) <| round <| CI.getX <| CI.getMember bin) gradesValues ] (CI.getBottom p bin) ]
                , C.yLabels [ CA.withGrid ]
                , C.bars
                    []
                    [ C.bar .y []
                    ]
                  <|
                    List.indexedMap
                        (\i v -> { x = toFloat i, y = Dict.get v grades |> Maybe.withDefault 0 |> toFloat })
                    <|
                        Array.toList gradesValues
                ]
        ]
