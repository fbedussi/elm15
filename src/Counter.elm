module Counter exposing (counter)

import Html exposing (..)
import Html.Attributes exposing (class)


splitAndConvertNumber : Int -> List String
splitAndConvertNumber number =
    number
        |> toString
        |> String.padLeft 2 '0'
        |> String.split ""


counter : Int -> Html msg
counter number =
    div [ class "counter" ] (List.map digit (splitAndConvertNumber number))


digit : String -> Html msg
digit digit =
    span
        [ class ("counter-digit digit-" ++ digit) ]
        [ div
            [ class "counter-digitInner" ]
            (List.map
                (\digit -> div [] [ text digit ])
                [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" ]
            )
        ]
