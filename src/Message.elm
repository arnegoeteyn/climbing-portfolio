module Message exposing (..)

import Browser
import Data exposing (ClimbingRoute)
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
    = JsonRequested
    | JsonSelected File
    | JsonLoaded String
    | ExportRequested
    | SaveRouteRequested
    | ToDatePicker DatePicker.Msg
      -- router
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url
      -- home
    | Home HomeMsg
      -- routes
    | ClimbingRoute ClimbingRouteMsg


type HomeMsg
    = None


type ClimbingRouteMsg
    = ClimbingRouteSelected ClimbingRoute
    | ShowNewRouteForm
    | CloseNewRouteForm
    | AddAscentButtonClicked
    | FormName String
    | FormGrade String
    | FormSector String
