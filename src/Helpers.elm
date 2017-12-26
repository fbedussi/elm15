module Helpers exposing (..)

import List.Extra exposing (elemIndex, swapAt)
import Models exposing (..)


isClickTileNextToZero : Int -> Tiles -> Bool
isClickTileNextToZero clickedTile tiles =
    let
        clickedTilePos =
            elemIndex clickedTile tiles
                |> Maybe.withDefault 0

        zeroPos =
            elemIndex 0 tiles
                |> Maybe.withDefault (List.length tiles)

        tilesPerRow =
            4

        clickedTileRow =
            clickedTilePos // tilesPerRow

        zeroRow =
            zeroPos // tilesPerRow

        clickedTileCol =
            clickedTilePos % tilesPerRow

        zeroCol =
            zeroPos % tilesPerRow
    in
    if
        clickedTileRow
            == zeroRow
            && abs (clickedTileCol - zeroCol)
            == 1
            || (clickedTileCol == zeroCol && abs (clickedTileRow - zeroRow) == 1)
    then
        True
    else
        False


swapTiles : Int -> Tiles -> Tiles
swapTiles clickedTile tiles =
    let
        clickedTilePos =
            Maybe.withDefault 0 (elemIndex clickedTile tiles)

        zeroPos =
            Maybe.withDefault 0 (elemIndex 0 tiles)
    in
    swapAt clickedTilePos zeroPos tiles
