import Html exposing (..)
import Html.Events exposing (..)
import Random exposing (generate)
import Random.List exposing (shuffle)

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


-- UPDATE
type Msg
  = Move
  | Scramble
  | NewSequence Tiles

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Scramble ->
      (model, generate NewSequence (shuffle model.tiles))

    NewSequence newSequence ->
      (Model newSequence, Cmd.none)

    Move ->
      (Model [1,3,2], Cmd.none)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW
view : Model -> Html Msg
view model =
    div []
    [ div [] (List.map (\val -> button [onClick Move] [text (toString val)]) model.tiles)
        , button [onClick Scramble] [text "scramble"]
    ]
