module Utilities.ItemFormUtilities exposing (..)

import Data exposing (Area, Ascent, ClimbingRoute, ClimbingRouteKind(..), ItemPageItem, Sector, ascentKindToString, climbingRouteKindToString, encodeClimbingRouteKind)
import Dict exposing (Dict)
import Model exposing (Criterium)
import Utilities exposing (maybeAccessor)


parentIdAccessor parentId =
    maybeAccessor (parentId >> Maybe.map String.fromInt >> Maybe.withDefault "")


toClimbingRouteFormCriteria : Maybe ClimbingRoute -> Dict String Criterium
toClimbingRouteFormCriteria maybeClimbingRoute =
    Dict.fromList
        [ ( "_parentId", { value = parentIdAccessor .sectorId maybeClimbingRoute, label = "_parentId", type_ = Model.Enumeration [] } )
        , ( "name", { value = maybeAccessor .name maybeClimbingRoute, label = "name", type_ = Model.String } )
        , ( "grade", { value = maybeAccessor .grade maybeClimbingRoute, label = "grade", type_ = Model.String } )
        , ( "description", { value = maybeAccessor (.description >> Maybe.withDefault "") maybeClimbingRoute, label = "description", type_ = Model.String } )
        , ( "kind"
          , { value =
                maybeAccessor
                    (.kind >> climbingRouteKindToString)
                    maybeClimbingRoute
            , label = "kind"
            , type_ = Model.Enumeration [ "sport", "boulder" ]
            }
          )
        ]


toSectorFormCriteria : Maybe Sector -> Dict String Criterium
toSectorFormCriteria maybeSector =
    Dict.fromList
        [ ( "_parentId", { value = parentIdAccessor .areaId maybeSector, label = "_parentId", type_ = Model.Enumeration [] } )
        , ( "name", { value = maybeAccessor .name maybeSector, label = "name", type_ = Model.String } )
        ]


toAreaFormCriteria : Maybe Area -> Dict String Criterium
toAreaFormCriteria maybeArea =
    Dict.fromList
        [ ( "name", { value = maybeAccessor .name maybeArea, label = "name", type_ = Model.String } )
        , ( "country", { value = maybeAccessor .country maybeArea, label = "country", type_ = Model.String } )
        ]


toAscentFormCriteria : Maybe Ascent -> Dict String Criterium
toAscentFormCriteria maybeAscent =
    Dict.fromList
        [ ( "_parentId", { value = parentIdAccessor .routeId maybeAscent, label = "_parentId", type_ = Model.Enumeration [] } )
        , ( "description", { value = maybeAccessor (.description >> Maybe.withDefault "") maybeAscent, label = "description", type_ = Model.String } )
        , ( "date", { value = maybeAccessor (.date >> Maybe.withDefault "") maybeAscent, label = "date", type_ = Model.Date } )
        , ( "kind"
          , { value =
                maybeAccessor
                    (.kind >> ascentKindToString)
                    maybeAscent
            , label = "kind"
            , type_ = Model.Enumeration [ "redpoint", "flash", "onsight", "secondgo", "repeat" ]
            }
          )
        ]
