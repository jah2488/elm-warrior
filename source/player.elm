module Player exposing (..)

import Common exposing (..)


newPlayer : Player
newPlayer =
    { hp = 100, facing = E, location = point 1 1 }
