module Common exposing (..)


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
