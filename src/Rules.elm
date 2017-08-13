module Rules exposing (..)

import Array exposing (Array)
import Maybe.Extra
import Random exposing (Seed)
import Types exposing (..)


{-|

    ## Rules for applying rules:

    - If a square has already updated, we return
    - Otherwise, go to the next rule in the list and maybe apply
    - One all rules have been applied, set the cells as not updated

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
    -- TODO: this is a mess
    indexedFoldl
        (\rowIndex row ( newGrid, seed ) ->
            let
                ( newRow, newSeed ) =
                    indexedFoldl
                        (\colIndex cell ( row, initialSeed ) ->
                            let
                                -- TODO: this is buggy, applies to already changed grid, not original grid
                                neighbors =
                                    gatherNeighbors grid
                                        rowIndex
                                        colIndex

                                ( newCell, newSeed ) =
                                    applyToCell
                                        neighbors
                                        initialSeed
                                        rule
                                        cell
                            in
                            ( Array.push newCell row, newSeed )
                         -- TODO: should not push into array
                        )
                        ( Array.empty, seed )
                        row
            in
            ( Array.push newRow newGrid, newSeed )
        )
        ( Array.empty, seed )
        grid


indexedFoldl : (Int -> a -> b -> b) -> b -> Array a -> b
indexedFoldl fn b array =
    array
        |> Array.indexedMap (\index a -> ( index, a ))
        |> Array.foldl (\( index, a ) b -> fn index a b) b


type alias Neighbors =
    List CellState


gatherNeighbors : Grid -> Int -> Int -> Neighbors
gatherNeighbors grid rowIndex colIndex =
    let
        neighborPositions =
            [ ( rowIndex - 1, colIndex - 1 )
            , ( rowIndex - 1, colIndex )
            , ( rowIndex - 1, colIndex + 1 )
            , ( rowIndex, colIndex - 1 )
            , ( rowIndex, colIndex + 1 )
            , ( rowIndex + 1, colIndex - 1 )
            , ( rowIndex + 1, colIndex )
            , ( rowIndex, colIndex + 1 )
            ]
    in
    neighborPositions
        |> List.map
            (\( row, col ) ->
                grid
                    |> Array.get row
                    |> Maybe.andThen (Array.get col)
            )
        |> Maybe.Extra.values
        |> List.map .state


applyToCell : Neighbors -> Seed -> Rule -> Cell -> ( Cell, Seed )
applyToCell neighbors seed rule cell =
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
                    applyToCell neighbors newSeed nestedRule cell
                else
                    ( cell, newSeed )

            IfCellIs cellState nestedRule ->
                if cell.state == cellState then
                    applyToCell neighbors seed nestedRule cell
                else
                    ( cell, seed )

            IfXNeighborsAre x cellState nestedRule ->
                let
                    matchingNeighbors =
                        neighbors
                            |> List.filter ((==) cellState)
                            |> List.length
                in
                if matchingNeighbors >= x then
                    applyToCell neighbors seed nestedRule cell
                else
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
