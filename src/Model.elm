module Model exposing (..)

import Browser.Navigation exposing (Key)
import Data exposing (Ascent, ClimbingRoute)
import Date exposing (Date)
import DatePicker
import Dict exposing (Dict)
import Message exposing (Route)
import Url exposing (Url)


type alias Model =
    { appState : AppState
    , url : Url -- not really used until we have something like /routes/1
    , route : Route
    , key : Key
    , climbingRoutes : Dict Int ClimbingRoute
    , ascents : Dict Int Ascent
    , climbingRoutesModel : ClimbingRoutesModel
    }


type AppState
    = NotReady
    | Ready


type alias ClimbingRoutesModel =
    { selectedRoute : Maybe ClimbingRoute -- todo to INT
    , form : Maybe ClimbingRouteForm
    , showNewAscentDate : Bool
    , datePicker : DatePicker.DatePicker
    , date : Maybe Date
    }


type alias ClimbingRouteForm =
    { name : String
    , grade : String
    }
