module Utilities exposing (..)

import Dict exposing (Dict)
import Html.Styled exposing (Html, input)
import Html.Styled.Attributes exposing (placeholder, type_, value)
import Html.Styled.Events exposing (onInput)
import Json.Encode


filterList : List ( a, Bool, Maybe a ) -> List a
filterList list =
    case list of
        [] ->
            []

        ( style, b, maybeAlternative ) :: xs ->
            if b then
                style :: filterList xs

            else
                case maybeAlternative of
                    Nothing ->
                        filterList xs

                    Just alternative ->
                        alternative :: filterList xs


encodeNullable : (value -> Json.Encode.Value) -> Maybe value -> Json.Encode.Value
encodeNullable valueEncoder maybeValue =
    case maybeValue of
        Just value ->
            valueEncoder value

        Nothing ->
            Json.Encode.null


newId : Dict Int a -> Int
newId dict =
    (Maybe.withDefault 1 <| List.head <| List.sortBy (\x -> x * -1) (Dict.keys dict)) + 1


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []
