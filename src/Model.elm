module Model exposing (..)

import Browser.Navigation exposing (Key)
import Data exposing (Area, Ascent, ClimbingRoute, Sector)
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
    , areas : Dict Int Area
    , climbingRoutesModel : ItemPageModel
    , sectorsModel : ItemPageModel
    , ascentsModel : ItemPageModel
    , areasModel : ItemPageModel
    }


type AppState
    = NotReady
    | Ready


type alias ItemPageModel =
    { itemType : Item
    , form : ItemPageItemForm
    , selectedItemId : Maybe Int
    }


type alias ItemPageItemForm =
    { criteria : Dict String Criterium
    , order : List String
    , parentId : Maybe String
    , formState : FormState
    }


type FormState
    = Hidden
    | Create
    | Update Int


type alias Criterium =
    { value : String
    , label : String
    }
