module Counter exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)

-- MODEL
type alias Model =
  Int

init : Model
init =
  0


-- UPDATE
type Msg
  = Increment
  | Decrement

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

-- VIEW
view : Model -> Html msg
view model =
    div [ class "counter" ]
    [ 
      text <| toString model
    ]
