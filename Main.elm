module Main exposing (..)

import Html exposing (Html, div, span, br, hr, h1, text)
import Html.App as App
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\n -> Sub.none)
        }


type Action
    = NoOp
    | Move Int Int


type Dir
    = N
    | E
    | S
    | W


type Space
    = Empty
    | Wall
    | Exit
    | NPC
    | Treasure
    | Enemy


type alias Point =
    { x : Int
    , y : Int
    }


type alias Room =
    { spaces : List (List Space)
    }


type alias Player =
    { hp : Float
    , facing : Dir
    , location : Point
    }


type alias Game =
    { room : Room
    , player : Player
    , score : Float
    }


point : Int -> Int -> Point
point row col =
    { x = col, y = row }


emptyRoom : Room
emptyRoom =
    { spaces =
        [ [ Wall, Wall, Wall, Wall, Wall ]
        , [ Wall, Empty, Empty, Exit, Wall ]
        , [ Wall, Empty, Empty, Empty, Wall ]
        , [ Wall, Empty, Empty, Empty, Wall ]
        , [ Wall, Wall, Wall, Wall, Wall ]
        ]
    }


newPlayer : Player
newPlayer =
    { hp = 100, facing = E, location = point 1 1 }


newGame : Game
newGame =
    { room = emptyRoom, player = newPlayer, score = 0 }


update : Action -> Game -> ( Game, Cmd Action )
update action game =
    case action of
        Move row col ->
            let
                player =
                    game.player
            in
                ( { game
                    | player =
                        { player
                            | location =
                                { x = col, y = row }
                        }
                  }
                , Cmd.none
                )

        NoOp ->
            ( game, Cmd.none )


init : ( Game, Cmd Action )
init =
    ( newGame
    , Cmd.none
    )


view : Game -> Html Action
view game =
    div []
        [ h1 [] [ text "Elm Warrior" ]
        , hr [] []
        , div [] (displayRoom game)
        ]


spaceToHtml : Player -> Int -> Int -> Space -> Html Action
spaceToHtml player row col space =
    let
        markerText =
            if player.location.x == col && player.location.y == row then
                "Player"
            else
                toString space
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
    List.concat <| List.indexedMap (rowToHtml player) room.spaces
