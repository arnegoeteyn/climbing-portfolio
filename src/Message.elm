module Message exposing (..)

import Browser
import Data exposing (Ascent, ClimbingRoute, Sector)
import DatePicker
import File exposing (File)
import Url exposing (Url)


type Item a
    = ClimbingRouteItem ClimbingRoute
    | AscentItem Ascent
    | SectorItem Sector


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
    | DataUpdate DataUpdateMsg


type HomeMsg
    = None


type DataUpdateMsg
    = CreateFromForm CreateFromFromMsg
    | OpenForm
    | CloseForm


type CreateFromFromMsg
    = ClimbingRouteForm
    | AscentForm
    | SectorForm


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
