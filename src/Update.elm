module Update exposing (update)

import Helpers exposing (isClickTileNextToZero, swapTiles)
import Init exposing (init)
import Models exposing (..)
import Msgs exposing (..)
import Navigation exposing (newUrl)
import Random exposing (generate)
import Random.List exposing (shuffle)
import Routing exposing (parseLocation)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLocation path ->
            ( model, newUrl path )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location

                log =
                    Debug.log "model" model
            in
            ( Model
                model.tiles
                model.turns
                model.success
                model.correctSequence
                newRoute
            , Cmd.none
            )

        Scramble ->
            ( resetModel model.route, generate NewSequence (shuffle model.tiles) )

        NewSequence newSequence ->
            ( Model
                newSequence
                model.turns
                (newSequence == model.correctSequence)
                model.correctSequence
                model.route
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
                    model.route
              else
                model
            , Cmd.none
            )

        Reset ->
            ( resetModel model.route, Cmd.none )
