module Display exposing (view)

import Common exposing (..)
import Html exposing (Html, div, span, br, hr, h1, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


view : Game -> Html Action
view game =
    div []
        [ h1 [] [ text "Elm Warrior" ]
        , hr [] []
        , div [] (displayRoom game)
        , hr [] []
        , h1 [ onClick Step ] [ text "STEP" ]
        , div [] [ text (toString game.player) ]
        ]


spaceToHtml : Player -> Int -> Int -> Space -> Html Action
spaceToHtml player row col space =
    let
        markerText =
            if player.location.x == col && player.location.y == row then
                "\xF8FF"
            else
                case space of
                    Empty ->
                        " . "

                    Wall ->
                        "#"

                    Exit ->
                        "[ ]"

                    NPC ->
                        "?"

                    Treasure ->
                        "!"

                    Enemy ->
                        "X"
    in
        span
            [ style
                [ ( "width", "50px" )
                , ( "display", "inline-block" )
                ]
            , onClick (Move row col)
            ]
            [ text markerText ]


rowToHtml : Player -> Int -> List Space -> List (Html Action)
rowToHtml player index row =
    [ br [] [] ] ++ (List.indexedMap (spaceToHtml player index) row)


displayRoom : Game -> List (Html Action)
displayRoom { room, player } =
    List.concat (List.indexedMap (rowToHtml player) room.spaces)
