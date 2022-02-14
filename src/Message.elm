module Message exposing (..)

import Browser
import Chart.Item as CI
import DatePicker
import File exposing (File)
import Url exposing (Url)


type Route
    = HomeRoute
    | AscentsRoute (Maybe Int) (Maybe String)
    | RoutesRoute (Maybe Int) (Maybe String) -- selected, criteria
    | SectorsRoute (Maybe Int) (Maybe String)
    | AreasRoute (Maybe Int) (Maybe String)
    | NotFoundRoute


type ItemType
    = ClimbingRouteItem
    | AscentItem
    | SectorItem
    | AreaItem


type alias ItemRelation =
    { parent : Maybe ItemType
    , child : Maybe ItemType
    }


type Msg
    = Dummy
    | JsonRequested
    | JsonSelected File
    | JsonLoaded String
    | ExportRequested
    | SaveItemRequested ItemType
    | DeleteItem ItemType Int
    | ToDatePicker ItemType String DatePicker.Msg
      -- router
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url
      -- Pages
    | Home HomeMsg
    | ItemPage ItemType ItemPageMsg


type HomeMsg
    = OnHover (List (CI.One { x : Float, y : Float, z : String } CI.Bar))


type ItemPageMsg
    = CreateNewItem
    | CloseForm
    | UpdateItem Int
    | SelectItem Int
    | FormUpdateMessage CriteriumUpdate
    | FilterUpdateMessage String String


type CriteriumUpdate
    = UpdateParent String
    | UpdateKey String String
