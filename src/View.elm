module View exposing (..)

import Counter exposing (counter)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Icons exposing (..)
import Json.Decode as Decode
import Models exposing (..)
import Msgs exposing (..)
import TouchEvents exposing (onTouchStart)


onLinkClick : msg -> Attribute msg
onLinkClick message =
    let
        options =
            { stopPropagation = False
            , preventDefault = True
            }
    in
    onWithOptions "click" options (Decode.succeed message)


view : Model -> Html Msg
view model =
    div []
        [ page model ]


page : Model -> Html Msg
page model =
    case model.route of
        Models.Home ->
            homePage model

        Models.Credits ->
            creditsPage ()

        Models.NotFoundRoute ->
            notFoundView


notFoundView : Html msg
notFoundView =
    div []
        [ text "Not found"
        ]


homePage : Model -> Html Msg
homePage model =
    let
        creditsPath =
            "credits"
    in
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
            , a
                [ class "linkBtn creditBtn"
                , href creditsPath
                , onLinkClick (ChangeLocation creditsPath)
                ]
                [ text "credits" ]
            ]
        ]


creditsPage : () -> Html Msg
creditsPage () =
    let
        homePath =
            "/"
    in
    div
        [ class "creditsPage" ]
        [ div
            [ class "creditsHeader" ]
            [ text "All icons are form the Noun Project" ]
        , ul
            [ class "iconsCredit" ]
            [ li
                [ class "iconsCredit-item" ]
                [ span
                    [ class "iconsCredit-icon" ]
                    [ resetIcon () ]
                , span
                    [ class "iconsCredit-text" ]
                    [ text "by Setyo Ari Wibowo" ]
                ]
            , li
                [ class "iconsCredit-item" ]
                [ span
                    [ class "iconsCredit-icon" ]
                    [ shuffleIcon () ]
                , span
                    [ class "iconsCredit-text" ]
                    [ text "by Ananth" ]
                ]
            , li 
                [class "iconsCredit-item"]
                [ span
                    [ class "iconsCredit-icon"]
                    [ backIcon () ] 
                , span 
                    [ class "iconsCredit-text"]
                    [ text "by praveen patchu"]
                ]
            ]
        , a
            [ class "linkBtn backToHome"
            , href homePath
            , onLinkClick (ChangeLocation homePath)
            ]
            [ backIcon () ]
        ]
