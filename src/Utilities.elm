module Utilities exposing (..)

import Date
import Dict exposing (Dict)
import Html.Styled exposing (Html, input)
import Html.Styled.Attributes exposing (placeholder, type_, value)
import Html.Styled.Events exposing (onInput)
import Json.Decode exposing (maybe)
import Json.Encode
import List exposing (sortBy)


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


ifNot : Maybe a -> Maybe a -> Maybe a
ifNot new maybe =
    case maybe of
        Just _ ->
            maybe

        Nothing ->
            new


maybeAccessor : (a -> String) -> Maybe a -> String
maybeAccessor accessor maybeValue =
    Maybe.withDefault "" <| Maybe.map accessor maybeValue


stringFromList : List String -> String
stringFromList =
    stringFromListWith ""


stringFromListWith : String -> List String -> String
stringFromListWith seperator list =
    case list of
        [] ->
            ""

        [ x ] ->
            x

        x :: xs ->
            x ++ seperator ++ stringFromListWith seperator xs


maybeDateToString : Maybe Date.Date -> String
maybeDateToString maybeDate =
    Maybe.map Date.toIsoString maybeDate |> Maybe.withDefault ""


sortByDescending : (a -> comparable) -> List a -> List a
sortByDescending func =
    List.sortWith
        (\a b ->
            case compare (func a) (func b) of
                LT ->
                    GT

                EQ ->
                    EQ

                GT ->
                    LT
        )


sortDescending : List comparable -> List comparable
sortDescending =
    sortByDescending identity
