module ApplyRules exposing (..)

import Array
import Types exposing (..)


{-|

    ## Rules for applying rules:

    - If a square has already updated, we return
    - Otherwise, go to the next rule in the list and maybe apply
    - One all rules have been applied, set the cells as not updated

-}
applyRules : List Rule -> Grid -> Grid
applyRules rules grid =
    List.foldl applyRule grid rules
        |> resetUpdateStatus


applyRule : Rule -> Grid -> Grid
applyRule rule grid =
    let
        applyToCell cell =
            if cell.updated then
                cell
            else
                case rule of
                    ChangeFromAToB x y ->
                        if cell.state == x then
                            { cell | state = y, updated = True }
                        else
                            cell
    in
    Array.map (Array.map applyToCell) grid


resetUpdateStatus : Grid -> Grid
resetUpdateStatus grid =
    Array.map (Array.map (\cell -> { cell | updated = False })) grid
