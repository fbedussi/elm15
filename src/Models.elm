module Models exposing (..)


type alias Tiles =
    List Int


type alias Model =
    { tiles : Tiles
    , turns : Int
    , success : Bool
    , correctSequence : Tiles
    , route : Route
    }


type Route
    = Home
    | Credits
    | NotFoundRoute


resetModel : Route -> Model
resetModel route =
    Model
        [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0 ]
        0
        False
        [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0 ]
        route
