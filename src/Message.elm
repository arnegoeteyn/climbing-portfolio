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


type Item
    = ClimbingRouteItem
    | AscentItem
    | SectorItem


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
    | ItemPage Item ItemPageMsg


type HomeMsg
    = None


type ItemPageMsg
    = OpenForm
    | CloseForm
    | SelectItem Int
    | FormUpdateMessage String String


type ClimbingRouteMsg
    = AddAscentButtonClicked
    | FormName String
    | FormGrade String
    | FormSector String


type SectorMsg
    = SectorSelected Sector
    | ShowNewSectorForm
    | CloseNewSectorForm
