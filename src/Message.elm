module Message exposing (..)

import Browser
import Data exposing (ClimbingRoute, ItemPageItem, Sector)
import DatePicker
import File exposing (File)
import Url exposing (Url)


type Route
    = HomeRoute
    | AscentsRoute
    | RoutesRoute
    | SectorsRoute
    | AreasRoute
    | NotFoundRoute


type Msg
    = Dummy
    | JsonRequested
    | JsonSelected File
    | JsonLoaded String
    | ExportRequested
    | SaveRouteRequested
    | ToDatePicker DatePicker.Msg
      -- router
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url
      -- Pages
    | Home HomeMsg
    | ClimbingRoute ClimbingRouteMsg
    | Sector SectorMsg
    | ItemPage ItemPageMsg Item


type HomeMsg
    = None


type ItemPageMsg
    = OpenForm
    | CloseForm
    | SelectItem Int


type Item
    = ClimbingRouteItem
    | AscentItem
    | SectorItem


type ClimbingRouteMsg
    = ClimbingRouteSelected ClimbingRoute
    | ShowNewRouteForm
    | CloseNewRouteForm
    | AddAscentButtonClicked
    | FormName String
    | FormGrade String
    | FormSector String


type SectorMsg
    = SectorSelected Sector
    | ShowNewSectorForm
    | CloseNewSectorForm
