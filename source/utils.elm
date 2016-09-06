module Utils exposing (..)


at : Int -> List a -> Maybe a
at idx list =
    List.head <| List.drop idx list
