module Example exposing (..)

import Expect exposing (Expectation)
--import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Fifteen exposing (..)


suite : Test
suite =
    describe "isClickTileNextToZero"
        [ test "click on a tile next to 0" <|
            \() ->
                Fifteen.isClickTileNextToZero 15 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0] |> Expect.equal True
        ]