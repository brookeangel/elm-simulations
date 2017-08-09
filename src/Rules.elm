module Rules exposing (..)

import Array exposing (Array)
import EveryDict exposing (EveryDict)
import ProbabilityGrid exposing (ProbabilityRuleGrids)
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


applyRules : ProbabilityRuleGrids Rule -> List Rule -> Grid -> Grid
applyRules probabilityGrids rules grid =
    List.foldl (applyRule probabilityGrids) grid rules
        |> resetUpdateStatus


applyRule : ProbabilityRuleGrids Rule -> Rule -> Grid -> Grid
applyRule probabilityGrids rule grid =
    Array.indexedMap
        (\rowIndex row ->
            Array.indexedMap
                (\colIndex cell ->
                    applyToCell rule probabilityGrids rowIndex colIndex cell
                )
                row
        )
        grid


applyToCell : Rule -> ProbabilityRuleGrids Rule -> Int -> Int -> Cell -> Cell
applyToCell rule probabilityGrids row column cell =
    if cell.updated then
        cell
    else
        case rule of
            ChangeToB b ->
                { cell | state = b, updated = True }

            Probability probability nestedRule ->
                -- Maybe this should all happen in the generator module.
                let
                    shouldApply =
                        applyProbabilityRule
                            { rule = rule
                            , probability = probability
                            , probabilityGrids = probabilityGrids
                            , row = row
                            , column = column
                            }
                in
                if shouldApply then
                    applyToCell nestedRule probabilityGrids row column cell
                else
                    cell

            IfCellIs cellState nestedRule ->
                if cell.state == cellState then
                    applyToCell nestedRule probabilityGrids row column cell
                else
                    cell


type alias ApplyProbabilityRuleConfig =
    { rule : Rule
    , probability : Float
    , probabilityGrids : ProbabilityRuleGrids Rule
    , row : Int
    , column : Int
    }


applyProbabilityRule : ApplyProbabilityRuleConfig -> Bool
applyProbabilityRule config =
    config.probabilityGrids
        |> EveryDict.get config.rule
        |> Maybe.andThen (Array.get config.row)
        |> Maybe.andThen (Array.get config.column)
        |> Maybe.map (\roll -> roll <= config.probability)
        |> Maybe.withDefault False


resetUpdateStatus : Grid -> Grid
resetUpdateStatus grid =
    Array.map (Array.map (\cell -> { cell | updated = False })) grid
