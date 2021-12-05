module Message exposing (..)

import Browser
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


type alias ItemRelation =
    { parent : Maybe Item
    , child : Maybe Item
    }


type Msg
    = Dummy
    | JsonRequested
    | JsonSelected File
    | JsonLoaded String
    | ExportRequested
    | SaveItemRequested Item
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
    | FormUpdateMessage CriteriumUpdate


type CriteriumUpdate
    = UpdateParent String
    | UpdateKey String String


type ClimbingRouteMsg
    = AddAscentButtonClicked


type SectorMsg
    = NoneSector
