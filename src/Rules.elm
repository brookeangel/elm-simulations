module Rules exposing (..)

import Array exposing (Array)
import Random exposing (Seed)
import Types exposing (..)


{-|

    ## Rules for applying rules:

    - If a square has already updated, we return
    - Otherwise, go to the next rule in the list and maybe apply
    - One all rules have been applied, set the cells as not updated

    ### Simple Rules

    - These are the base case for rules
    - A Simple rule is applied unconditionally

    ### Non-Simple Rules

    - A complex rule contains a SimpleRule
    - Apply the embedded SimpleRule conditionally

-}
type Rule
    = ChangeToB CellState
    | Probability Float Rule
    | IfCellIs CellState Rule
    | IfXNeighborsAre Int CellState Rule


applyRules : Seed -> List Rule -> Grid -> ( Grid, Seed )
applyRules seed rules grid =
    List.foldl applyRule ( grid, seed ) rules
        |> Tuple.mapFirst resetUpdateStatus


applyRule : Rule -> ( Grid, Seed ) -> ( Grid, Seed )
applyRule rule ( grid, seed ) =
    -- TODO: clean me up
    Array.foldl
        (\row ( grid, seed ) ->
            let
                ( newRow, newSeed ) =
                    Array.foldl
                        (\cell ( row, initialSeed ) ->
                            let
                                ( newCell, newSeed ) =
                                    applyToCell initialSeed rule cell
                            in
                            ( Array.push newCell row, newSeed )
                        )
                        ( Array.empty, seed )
                        row
            in
            ( Array.push newRow grid, newSeed )
        )
        ( Array.empty, seed )
        grid


applyToCell : Seed -> Rule -> Cell -> ( Cell, Seed )
applyToCell seed rule cell =
    if cell.updated then
        ( cell, seed )
    else
        case rule of
            ChangeToB b ->
                ( { cell | state = b, updated = True }, seed )

            Probability probability nestedRule ->
                let
                    ( shouldApply, newSeed ) =
                        applyProbabilityRule
                            { rule = rule
                            , probability = probability
                            , seed = seed
                            }
                in
                if shouldApply then
                    applyToCell newSeed nestedRule cell
                else
                    ( cell, newSeed )

            IfCellIs cellState nestedRule ->
                if cell.state == cellState then
                    applyToCell seed nestedRule cell
                else
                    ( cell, seed )

            IfXNeighborsAre x cellState nestedRule ->
                ( cell, seed )


type alias ApplyProbabilityRuleConfig =
    { rule : Rule
    , probability : Float
    , seed : Seed
    }


applyProbabilityRule : ApplyProbabilityRuleConfig -> ( Bool, Seed )
applyProbabilityRule config =
    config.seed
        |> Random.step (Random.float 0 1)
        |> Tuple.mapFirst (\roll -> roll <= config.probability)


resetUpdateStatus : Grid -> Grid
resetUpdateStatus grid =
    Array.map (Array.map (\cell -> { cell | updated = False })) grid
