module Routing exposing (..)

import Debug
import Models exposing (Route(..))
import Navigation exposing (Location)
import UrlParser exposing (..)


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map Home top
        , map Home (s "home")
        , map Credits (s "credits")
        , map Credits (s "elm15" </> s "credits")
        ]


parseLocation : Location -> Route
parseLocation location =
    case parsePath matchers location of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
