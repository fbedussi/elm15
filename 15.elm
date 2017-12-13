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
    tiles: Tiles
   }

init : (Model, Cmd Msg)
init =
  (Model [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0], Cmd.none)


-- HELPERS
move : Tiles -> Tiles
move tiles =
  [1,2,3,4,5,6,7,8,9,10,11,12,13,14,0,15]

fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"

isClickTileNextToZero : Int -> Model -> Bool
isClickTileNextToZero clickedTile model =
  let
    clickedTilePos = fromJust (elemIndex clickedTile model.tiles)
    zeroPos = fromJust (elemIndex 0 model.tiles)
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

swapTiles : Int -> Model -> Model
swapTiles clickedTile model = 
  let
    clickedTilePos = fromJust (elemIndex clickedTile model.tiles)
    zeroPos = fromJust (elemIndex 0 model.tiles)
  in
    Model <| swapAt clickedTilePos zeroPos model.tiles

-- UPDATE
type Msg
  = Move Int
  | Scramble
  | NewSequence Tiles

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Scramble ->
      (model, generate NewSequence (shuffle model.tiles))

    NewSequence newSequence ->
      (Model newSequence, Cmd.none)

    Move clickedTile ->
        (if isClickTileNextToZero clickedTile model then swapTiles clickedTile model else model, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW
containerStyle : Attribute msg
containerStyle =
  style
    [ ("width", "400px") ]

tileStyle : Attribute msg
tileStyle =
  style
    [ ("width", "100px") 
      ,("height", "100px")
      ,("padding", "0")
      ,("box-sizing", "border-box")
      ,("vertical-align", "top")
    ]

view : Model -> Html Msg
view model =
    div [ containerStyle ]
    [ div [] (List.map (\val -> button [tileStyle, onClick (Move val)] [text (if val /= 0 then toString val else "")]) model.tiles)
        , button [onClick Scramble] [text "scramble"]
    ]
