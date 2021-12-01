module Data.ClimbingRoute exposing (ClimbingRoute)


type alias ClimbingRoute =
    { id : Int
    , name : String
    , grade : String
    , description : Maybe String
    , ascentIds : Maybe (List Int)
    }
