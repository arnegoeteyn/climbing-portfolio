module Model exposing (..)

import Browser.Navigation exposing (Key)
import Data exposing (Ascent, ClimbingRoute, Sector)
import Date exposing (Date)
import DatePicker
import Dict exposing (Dict)
import Message exposing (Item, Route)
import Url exposing (Url)


type alias Model =
    { appState : AppState
    , url : Url -- not really used until we have something like /routes/1
    , route : Route
    , key : Key
    , climbingRoutes : Dict Int ClimbingRoute
    , ascents : Dict Int Ascent
    , sectors : Dict Int Sector
    , climbingRoutesModel : ItemPageModel
    , sectorsModel : ItemPageModel
    , ascentsModel : ItemPageModel
    }


type AppState
    = NotReady
    | Ready


type alias ClimbingRoutesModel =
    { selectedRoute : Maybe ClimbingRoute -- todo to INT
    , form : Maybe ItemPageItemForm
    , showNewAscentDate : Bool
    , datePicker : DatePicker.DatePicker
    , date : Maybe Date
    }


type alias SectorsModel =
    { selectedSector : Maybe Sector -- todo to INT
    }


type alias ItemPageModel =
    { itemType : Item
    , form : Maybe ItemPageItemForm
    , selectedItemId : Maybe Int
    }


type alias ItemPageItemForm =
    { name : String
    , grade : String
    , sectorId : String
    }
