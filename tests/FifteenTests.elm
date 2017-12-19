module FifteenTests exposing (..)

import Expect exposing (Expectation)
--import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Fifteen exposing (..)


suite : Test
suite =
    describe "isClickTileNextToZero"
        [ test "click on a tile next to 0" <|
            \() ->
                Fifteen.isClickTileNextToZero 15 
                [ 1, 2, 3, 4,
                  5, 6, 7, 8,
                  9,10,11,12,
                 13,14,15, 0] |> Expect.equal True
        , test "click on a tile NOT next to 0" <|
            \() ->
                Fifteen.isClickTileNextToZero 1 
                [ 1, 2, 3, 4,
                  5, 6, 7, 8,
                  9,10,11,12,
                 13,14,15, 0] |> Expect.equal False
        , test "click on a tile next to 0 on another line" <|
            \() ->
                Fifteen.isClickTileNextToZero 12
                [ 1, 2, 3, 4,
                  5, 6, 7, 8,
                  9,10,11,12,
                 13,14,15, 0] |> Expect.equal True
        , test "click on a tile next to 0 when 0 is not in its starting position" <|
            \() ->
                Fifteen.isClickTileNextToZero 11
                [ 1, 2, 3, 4,
                  5, 6, 0, 8,
                  9,10,11,12,
                 13,14,15, 7] |> Expect.equal True
        ]