module Main exposing (..)

import Common exposing (..)
import Player exposing (..)
import Display
import Utils
import Html exposing (Html)
import Html.App as App


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\n -> Sub.none)
        }


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


newGame : Game
newGame =
    { room = emptyRoom, player = newPlayer, score = 0 }


spaceAt : Room -> Point -> Space
spaceAt room loc =
    let
        row =
            Utils.at loc.y room.spaces
    in
        case row of
            Just rx ->
                let
                    r =
                        Utils.at loc.x rx
                in
                    case r of
                        Just c ->
                            c

                        Nothing ->
                            Wall

            Nothing ->
                Wall


stepPlayer : Player -> Room -> Player
stepPlayer player room =
    let
        x =
            player.location.x

        y =
            player.location.y

        testPoint =
            \point' ->
                if (spaceAt room point' == Empty) then
                    point'
                else
                    player.location

        newLocation =
            case player.facing of
                N ->
                    testPoint <| point (x) (y - 1)

                E ->
                    testPoint <| point (x + 1) (y)

                S ->
                    testPoint <| point x (y + 1)

                W ->
                    testPoint <| point (x - 1) (y)
    in
        { player | location = newLocation }


stepGame : Game -> Game
stepGame game =
    { game | player = (stepPlayer game.player game.room) }


update : Action -> Game -> ( Game, Cmd Action )
update action game =
    case action of
        Step ->
            ( stepGame game, Cmd.none )

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
    Display.view game
