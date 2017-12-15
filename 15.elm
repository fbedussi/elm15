import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Random exposing (generate)
import Random.List exposing (shuffle)
import List.Extra exposing (elemIndex, swapAt)

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
  {
    tiles: Tiles,
    turns: Int,
    success: Bool,
    correctSequence: Tiles
   }

init : (Model, Cmd Msg)
init =
  (Model 
    [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0]
    0
    False
    [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0]
  , Cmd.none)


-- HELPERS
isClickTileNextToZero : Int -> Model -> Bool
isClickTileNextToZero clickedTile model =
  let
    clickedTilePos = elemIndex clickedTile model.tiles |> Maybe.withDefault 0
    zeroPos = elemIndex 0 model.tiles |> Maybe.withDefault (List.length model.tiles)
    tilesPerRow = 4
    clickedTileRow = clickedTilePos // tilesPerRow
    zeroRow = zeroPos // tilesPerRow
    clickedTileCol = clickedTilePos % tilesPerRow
    zeroCol = zeroPos % tilesPerRow
  in
    if 
      clickedTileRow == zeroRow && abs (clickedTileCol - zeroCol) == 1 
      || (clickedTileCol == zeroCol && abs (clickedTileRow - zeroRow) == 1)
    then 
      True
    else
      False

swapTiles : Int -> Tiles -> Tiles
swapTiles clickedTile tiles = 
  let
    clickedTilePos = Maybe.withDefault 0 (elemIndex clickedTile tiles)
    zeroPos = Maybe.withDefault 0 (elemIndex 0 tiles)
  in
    swapAt clickedTilePos zeroPos tiles

-- UPDATE
type Msg
  = Move Int
  | Scramble
  | NewSequence Tiles
  | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Scramble ->
      (model, generate NewSequence (shuffle model.tiles))

    NewSequence newSequence ->
      (Model
        newSequence
        model.turns
        (newSequence == model.correctSequence)
        model.correctSequence
      , Cmd.none)

    Move clickedTile ->    
        (
          if 
            isClickTileNextToZero clickedTile model 
          then
            let 
              newTiles = swapTiles clickedTile model.tiles
            in
              Model 
                newTiles
                (model.turns + 1)
                (newTiles == model.correctSequence)
                model.correctSequence
          else
            model
        , Cmd.none)
    
    Reset ->
      init

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW
containerStyle : Attribute msg
containerStyle =
  style
    [ ("width", "80vmin")
      ,("margin", "0 auto")
     ]

tileStyle : Attribute msg
tileStyle =
  style
    [ ("width", "20vmin") 
      ,("height", "20vmin")
      ,("padding", "0")
      ,("box-sizing", "border-box")
      ,("vertical-align", "top")
      ,("font-size", "2rem")
    ]

view : Model -> Html Msg
view model =
    div [ containerStyle ]
    [ 
      div [style [("display", (if model.success then "block" else "none"))]] [ text "Complete!" ]
      ,div [] [ text <| toString model.turns ]
      ,div [] (List.map (\val -> button [tileStyle, onClick (Move val)] [text (if val /= 0 then toString val else "")]) model.tiles)
      ,button [onClick Scramble] [text "scramble"]
      ,button [onClick Reset] [text "reset"]
    ]
