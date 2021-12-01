module Data.Ascent exposing (..)

import Data.ClimbingRoute exposing (ClimbingRoute)


type alias Ascent =
    { id : Int
    , routeId : Int
    , date : String
    }


type AscentKind
    = Onsight
    | Flash
    | SecondGo
    | Redpoint
    | Repeat
