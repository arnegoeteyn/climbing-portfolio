module Page.Home exposing (Model, Msg, init, update, view)

import Data.ApplicationData exposing (ApplicationData, updateRoutes)
import Data.Ascent exposing (Ascent)
import Data.ClimbingRoute exposing (ClimbingRoute)
import Date exposing (Date, day, month, weekday, year)
import DatePicker
import Dict exposing (Dict, values)
import Html
import Html.Styled exposing (Html, button, div, footer, header, img, input, p, text, ul)
import Html.Styled.Attributes exposing (css, placeholder, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Tailwind.Utilities as Tw


type alias Model =
    { selectedRoute : Maybe ClimbingRoute
    , form : Maybe ClimbingRouteForm
    , showNewAscentDate : Bool
    , datePicker : DatePicker.DatePicker
    }


type alias ClimbingRouteForm =
    { name : String
    , grade : String
    }


view : ApplicationData -> Model -> Html Msg
view applicationData model =
    let
        addRouteButton =
            case model.form of
                Nothing ->
                    button [ onClick ShowNewRouteForm ] [ text "add route" ]

                Just form ->
                    div []
                        [ button [ onClick CloseNewRouteForm ] [ text "close form" ]
                        , button [ onClick <| SaveNewRouteForm form ] [ text "save form" ]
                        ]
    in
    div []
        [ text "Home"
        , addRouteButton
        , viewRouteForm model.form
        , div
            [ Html.Styled.Attributes.css [ Tw.grid, Tw.grid_cols_2 ] ]
            [ ul [] <|
                List.map viewClimbingRoute <|
                    values applicationData.routes
            , div [ css [ Tw.flex, Tw.justify_center ] ] [ viewSelectedRoute applicationData model ]
            ]
        ]


viewClimbingRoute : ClimbingRoute -> Html Msg
viewClimbingRoute route =
    Html.Styled.li
        [ onClick <| ClimbingRouteSelected route ]
        [ text <| route.name ++ " " ++ route.grade ]


viewSelectedRoute : ApplicationData -> Model -> Html Msg
viewSelectedRoute applicationData model =
    case model.selectedRoute of
        Just route ->
            let
                cardTailwindProperties =
                    Html.Styled.Attributes.css [ Tw.rounded_xl, Tw.shadow_lg, Tw.bg_purple_100 ]

                cardHeaderImageTWProperties =
                    Html.Styled.Attributes.css [ Tw.rounded_t_lg, Tw.h_60, Tw.w_full, Tw.object_cover ]

                cardHeaderTextTWProperties =
                    Html.Styled.Attributes.css [ Tw.text_xl, Tw.font_extrabold, Tw.p_4 ]

                cardContentTWProperties =
                    Html.Styled.Attributes.css [ Tw.px_5 ]

                cardDescriptionTWProperties =
                    Html.Styled.Attributes.css [ Tw.px_4, Tw.text_gray_400 ]

                cardFooterTWProperties =
                    Html.Styled.Attributes.css [ Tw.text_right, Tw.py_3, Tw.px_8, Tw.text_gray_400 ]

                cardButtonTWProperties =
                    -- hover:bg-green-600
                    Html.Styled.Attributes.css [ Tw.py_2, Tw.px_4, Tw.mt_5, Tw.bg_green_500, Tw.rounded_lg, Tw.text_white, Tw.font_semibold ]
            in
            div [ cardTailwindProperties ]
                [ img
                    [ cardHeaderImageTWProperties
                    , Html.Styled.Attributes.src "https://www.barcelona-tourist-guide.com/images/ext/attractions/montserrat/L550/montserrat-barcelona-29.jpg"
                    ]
                    []
                , header [ cardHeaderTextTWProperties ] [ text <| route.name ++ " [" ++ route.grade ++ "]" ]
                , div [ cardContentTWProperties ] [ p [ cardDescriptionTWProperties ] [ text <| Maybe.withDefault "" route.description ], viewAscents applicationData route model ]
                , footer [ cardFooterTWProperties ] [ button [ cardButtonTWProperties ] [ text "edit" ] ]
                ]

        Nothing ->
            div [] []


viewRouteForm : Maybe ClimbingRouteForm -> Html Msg
viewRouteForm maybeForm =
    case maybeForm of
        Nothing ->
            text ""

        Just form ->
            div []
                [ viewInput "text" "Name" form.name FormName
                , viewInput "text" "Grade" form.grade FormGrade
                ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewAscents : ApplicationData -> ClimbingRoute -> Model -> Html Msg
viewAscents applicationData route model =
    let
        ascents =
            List.filterMap identity <| List.map (\id -> Dict.get id applicationData.ascents) (Maybe.withDefault [] route.ascentIds)

        viewAscent ascent =
            div [] [ text ascent.date ]

        datePickerDialog =
            DatePicker.view Nothing DatePicker.defaultSettings model.datePicker
                |> Html.Styled.fromUnstyled
                |> Html.Styled.map ToDatePicker
    in
    div []
        [ text <| String.fromInt (List.length ascents) ++ " ascents!"
        , button [ onClick (AddAscentButtonClicked route) ] [ text "+" ]
        , if model.showNewAscentDate then
            datePickerDialog

          else
            text ""
        , div [] <| List.map viewAscent ascents
        ]


type Msg
    = ClimbingRouteSelected ClimbingRoute
    | ShowNewRouteForm
    | CloseNewRouteForm
    | SaveNewRouteForm ClimbingRouteForm
    | AddAscentButtonClicked ClimbingRoute
    | FormName String
    | FormGrade String
    | ToDatePicker DatePicker.Msg


init : ( Model, Cmd Msg )
init =
    let
        ( datePicker, datePickerFx ) =
            DatePicker.init
    in
    ( { selectedRoute = Nothing
      , form = Nothing
      , showNewAscentDate = False
      , datePicker = datePicker
      }
    , Cmd.map ToDatePicker datePickerFx
    )


initForm : ClimbingRouteForm
initForm =
    { name = "", grade = "" }


update : ApplicationData -> Msg -> Model -> ( Model, Cmd Msg, ApplicationData )
update applicationData msg model =
    case msg of
        ClimbingRouteSelected route ->
            ( { model | selectedRoute = Just route }, Cmd.none, applicationData )

        ShowNewRouteForm ->
            ( { model | form = Just initForm }, Cmd.none, applicationData )

        CloseNewRouteForm ->
            ( { model | form = Nothing }, Cmd.none, applicationData )

        AddAscentButtonClicked route ->
            ( { model | showNewAscentDate = not model.showNewAscentDate }, Cmd.none, applicationData )

        SaveNewRouteForm form ->
            let
                newRoutes applicationRoutes =
                    updateRoutes (formToClimbingRoute applicationData form) applicationRoutes

                newApplicationData =
                    { applicationData | routes = newRoutes applicationData.routes }
            in
            update newApplicationData CloseNewRouteForm model

        FormName name ->
            let
                formUpdate form =
                    Just { form | name = name }

                newForm =
                    Maybe.andThen formUpdate model.form
            in
            ( { model | form = newForm }, Cmd.none, applicationData )

        FormGrade grade ->
            let
                newForm =
                    Maybe.andThen (\form -> Just { form | grade = grade }) model.form
            in
            ( { model | form = newForm }, Cmd.none, applicationData )

        ToDatePicker subMsg ->
            case model.selectedRoute of
                Nothing ->
                    ( model, Cmd.none, applicationData )

                Just selectedRoute ->
                    let
                        ( newDatePicker, event ) =
                            DatePicker.update DatePicker.defaultSettings subMsg model.datePicker

                        datePicked =
                            case event of
                                DatePicker.Picked date ->
                                    Just date

                                _ ->
                                    Nothing

                        newAscentId =
                            newId applicationData.ascents

                        newRoutes =
                            case datePicked of
                                Nothing ->
                                    applicationData.routes

                                Just date ->
                                    let
                                        modifiedRoute : ClimbingRoute
                                        modifiedRoute =
                                            { selectedRoute | ascentIds = Just <| newAscentId :: Maybe.withDefault [] selectedRoute.ascentIds }
                                    in
                                    Dict.insert selectedRoute.id modifiedRoute applicationData.routes

                        newAscents =
                            case datePicked of
                                Nothing ->
                                    applicationData.ascents

                                Just date ->
                                    let
                                        newAscent : Ascent
                                        newAscent =
                                            { id = newAscentId, routeId = selectedRoute.id, date = Date.toIsoString date }
                                    in
                                    Dict.insert newAscent.id newAscent applicationData.ascents
                    in
                    ( { model
                        | showNewAscentDate =
                            case datePicked of
                                Just date ->
                                    False

                                Nothing ->
                                    model.showNewAscentDate
                        , datePicker = newDatePicker
                      }
                    , Cmd.none
                    , { applicationData | ascents = newAscents, routes = newRoutes }
                    )


formToClimbingRoute : ApplicationData -> ClimbingRouteForm -> ClimbingRoute
formToClimbingRoute data form =
    { name = form.name, grade = form.grade, description = Just "dit is nieuw", id = newId data.routes, ascentIds = Just [] }


newId : Dict Int a -> Int
newId dict =
    (Maybe.withDefault 1 <| List.head <| List.sortBy (\x -> x * -1) (Dict.keys dict)) + 1
