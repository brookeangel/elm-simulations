module ApplyRules exposing (..)

import Array
import Types exposing (..)


applyRules : List Rule -> Grid -> Grid
applyRules rules grid =
    List.foldl applyRule grid rules


applyRule : Rule -> Grid -> Grid
applyRule rule grid =
    let
        applyToCell cell =
            case rule of
                ChangeFromAToB x y ->
                    if cell == x then
                        y
                    else
                        cell
    in
    Array.map (Array.map applyToCell) grid
