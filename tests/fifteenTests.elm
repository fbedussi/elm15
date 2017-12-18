module FifteenTests exposing (main)

import Test exposing (describe, test)
import Expect
import Test.Runner.Html exposing (run)
import Aaa exposing (..)


main =
    run <|
        describe "isClickTileNextToZero"
            [ test "click on a tile next to 0" <|
                \() ->
                    isClickTileNextToZero 15 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0] |> Expect.equal [1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,15]
            ]