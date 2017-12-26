module Fifteen exposing (..)

import Init exposing (init)
import Models exposing (Model, resetModel)
import Msgs exposing (Msg)
import Navigation
import Subscriptions exposing (subscriptions)
import Update exposing (update)
import View exposing (view)


main : Program Never Model Msg
main =
    Navigation.program Msgs.OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
