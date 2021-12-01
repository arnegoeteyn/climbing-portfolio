module Page.Stats exposing (Model, Msg, init, update, view)

import Data.ApplicationData exposing (ApplicationData)
import Dict
import Html.Styled exposing (Html, text)


type alias Model =
    {}


view : ApplicationData -> Model -> Html Msg
view data model =
    let
        climbedRoutes =
            List.map .ascentIds (Dict.values data.routes)
                |> List.filterMap identity
                |> List.filter (List.isEmpty >> not)
    in
    text <| "You climbed " ++ (String.fromInt <| List.length climbedRoutes) ++ " routes!"


type Msg
    = Todo


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
