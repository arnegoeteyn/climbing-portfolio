module Model exposing (..)

import Browser.Navigation exposing (Key)
import Chart.Item as CI
import Data exposing (Area, Ascent, ClimbingRoute, Sector)
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
    , homeModel : HomeModel
    , climbingRoutesModel : ItemPageModel
    , sectorsModel : ItemPageModel
    , ascentsModel : ItemPageModel
    , areasModel : ItemPageModel
    , datePicker : DatePicker.DatePicker
    }


type AppState
    = NotReady
    | Ready


type alias ItemPageModel =
    { itemType : Item
    , form : ItemPageItemForm
    , selectedItemId : Maybe Int
    , filters : Dict String String
    , sortOnColumn : Maybe Int
    }


type alias HomeModel =
    { hovering : List (CI.One { x : Float, y : Float, z : String } CI.Bar)
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


type CriteriumType
    = String
    | Date
    | Enumeration (List String)


type alias Criteria =
    Dict String Criterium


type alias Criterium =
    { value : String
    , label : String
    , type_ : CriteriumType
    }
