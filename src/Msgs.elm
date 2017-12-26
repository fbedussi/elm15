module Msgs exposing (..)

import Models exposing (Tiles)
import Navigation exposing (Location)


type Msg
    = Move Int
    | Scramble
    | NewSequence Tiles
    | Reset
    | OnLocationChange Location
    | ChangeLocation String
