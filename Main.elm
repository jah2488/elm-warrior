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
    | Step


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
point col row =
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


at : Int -> List a -> Maybe a
at idx list =
    List.head <| List.drop idx list


spaceAt : Room -> Point -> Space
spaceAt room loc =
    let
        row =
            at loc.y room.spaces
    in
        case row of
            Just rx ->
                let
                    r =
                        at loc.x rx
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

        newLocation =
            case player.facing of
                N ->
                    if (spaceAt room (point x (y - 1)) == Empty) then
                        point (x) (y - 1)
                    else
                        player.location

                E ->
                    point (x + 1) (y)

                S ->
                    point x (y + 1)

                W ->
                    point (x - 1) (y)
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
