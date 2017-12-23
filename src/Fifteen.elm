module Fifteen exposing (..)

import Counter exposing (counter)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icons exposing (..)
import List.Extra exposing (elemIndex, swapAt)
import Random exposing (generate)
import Random.List exposing (shuffle)
import TouchEvents exposing (onTouchStart)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Tiles =
    List Int


type alias Model =
    { tiles : Tiles
    , turns : Int
    , success : Bool
    , correctSequence : Tiles
    }


resetModel : () -> Model
resetModel () =
    Model
        [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0 ]
        0
        False
        [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 0 ]


init : ( Model, Cmd Msg )
init =
    ( resetModel ()
    , Cmd.none
    )



-- HELPERS


isClickTileNextToZero : Int -> Tiles -> Bool
isClickTileNextToZero clickedTile tiles =
    let
        clickedTilePos =
            elemIndex clickedTile tiles |> Maybe.withDefault 0

        zeroPos =
            elemIndex 0 tiles |> Maybe.withDefault (List.length tiles)

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



-- UPDATE


type Msg
    = Move Int
    | Scramble
    | NewSequence Tiles
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Scramble ->
            ( resetModel (), generate NewSequence (shuffle model.tiles) )

        NewSequence newSequence ->
            ( Model
                newSequence
                model.turns
                (newSequence == model.correctSequence)
                model.correctSequence
            , Cmd.none
            )

        Move clickedTile ->
            ( if isClickTileNextToZero clickedTile model.tiles then
                let
                    newTiles =
                        swapTiles clickedTile model.tiles

                    updatedCounter =
                        model.turns + 1
                in
                Model
                    newTiles
                    updatedCounter
                    (newTiles == model.correctSequence)
                    model.correctSequence
              else
                model
            , Cmd.none
            )

        Reset ->
            init



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ div
            [ class "success"
            , style
                [ ( "display"
                  , if model.success then
                        "block"
                    else
                        "none"
                  )
                ]
            ]
            [ div [ class "okIcon" ] [ okIcon () ] ]
        , div
            [ class "board" ]
            (List.map
                (\val ->
                    button
                        [ class
                            (if val /= 0 then
                                "tile"
                             else
                                "tile is-empty"
                            )
                        , onTouchStart (\_ -> Move val)
                        , onClick (Move val)
                        ]
                        [ text
                            (if val /= 0 then
                                toString val
                             else
                                ""
                            )
                        ]
                )
                model.tiles
            )
        , div [ class "controls" ]
            [ div [ class "turnsWrapper" ]
                [ counter model.turns
                ]
            , div [ class "btnWrapper" ]
                [ button [ class "btn scrambleBtn", onClick Scramble ] [ shuffleIcon () ]
                , button [ class "btn resetBtn", onClick Reset ] [ resetIcon () ]
                ]
            ]
        ]
