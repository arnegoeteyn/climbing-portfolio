module Model exposing (..)

import Browser.Navigation exposing (Key)
import Chart.Item as CI
import Data exposing (Area, Ascent, ClimbingRoute, Sector, Trip)
import DatePicker
import Dict exposing (Dict)
import Message exposing (ItemType, Route)
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
    , trips : Dict Int Trip
    , homeModel : HomeModel
    , climbingRoutesModel : ItemPageModel
    , sectorsModel : ItemPageModel
    , ascentsModel : ItemPageModel
    , tripsModel : ItemPageModel
    , areasModel : ItemPageModel
    , datePicker : DatePicker.DatePicker
    }


type AppState
    = NotReady
    | Ready


type alias Filter =
    ItemType -> String -> Bool


type alias ItemPageModel =
    { itemType : ItemType
    , form : EntityForm
    , selectedItemId : Maybe Int
    , filterValues : Dict String String
    , sortOnColumn : Maybe Int
    }


type alias HomeModel =
    { hovering : List (CI.One { x : Float, y : Float, z : String } CI.Bar)
    }


type alias EntityForm =
    { criteria : Dict String Criterium
    , order : List String
    , parentId : Maybe String
    , formState : FormState
    , entity : Entity
    }


type alias Entity =
    { itemType : ItemType, id : Maybe Int }


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
