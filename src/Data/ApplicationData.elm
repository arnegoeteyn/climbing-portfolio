module Data.ApplicationData exposing (..)

import Data.Ascent exposing (Ascent)
import Data.ClimbingRoute exposing (ClimbingRoute)
import Dict exposing (Dict)


type alias ApplicationData =
    { routes : Dict Int ClimbingRoute
    , ascents : Dict Int Ascent
    }


updateRoutes : ClimbingRoute -> Dict Int ClimbingRoute -> Dict Int ClimbingRoute
updateRoutes newRoute =
    Dict.insert newRoute.id newRoute
