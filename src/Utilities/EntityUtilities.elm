module Utilities.EntityUtilities exposing (..)

import Message exposing (ItemType(..))


getChild : ItemType -> Maybe ItemType
getChild type_ =
    case type_ of
        AreaItem ->
            Just SectorItem

        SectorItem ->
            Just ClimbingRouteItem

        ClimbingRouteItem ->
            Just AscentItem

        AscentItem ->
            Nothing


getParent : ItemType -> Maybe ItemType
getParent type_ =
    case type_ of
        AreaItem ->
            Nothing

        SectorItem ->
            Just AreaItem

        ClimbingRouteItem ->
            Just SectorItem

        AscentItem ->
            Just ClimbingRouteItem
