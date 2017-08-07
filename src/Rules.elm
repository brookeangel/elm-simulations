module Rules exposing (..)

import Array exposing (Array)
import EveryDict exposing (EveryDict)
import List.Extra
import ProbabilityGrid exposing (ProbabilityRuleGrids)
import Random exposing (Generator)
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
    = Simple SimpleRule
    | Probability Float SimpleRule


type SimpleRule
    = ChangeFromAToB CellState CellState


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
                    applyToCell rule probabilityGrids colIndex rowIndex cell
                )
                row
        )
        grid


applyToCell : Rule -> ProbabilityRuleGrids Rule -> Int -> Int -> Cell -> Cell
applyToCell rule probabilityGrids column row cell =
    -- TODO: i'm sorry for this horrendous code I'm so sorry
    -- Maybe This should all happen in the generator module.
    if cell.updated then
        cell
    else
        case rule of
            Simple simpleRule ->
                applySimpleRule simpleRule cell

            Probability probability simpleRule ->
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
                    applySimpleRule simpleRule cell
                else
                    cell


applySimpleRule : SimpleRule -> Cell -> Cell
applySimpleRule rule cell =
    case rule of
        ChangeFromAToB x y ->
            if cell.state == x then
                { cell | state = y, updated = True }
            else
                cell


type alias ApplyRuleConfig =
    { rule : Rule
    , probability : Float
    , probabilityGrids : ProbabilityRuleGrids Rule
    , row : Int
    , column : Int
    }


applyProbabilityRule : ApplyRuleConfig -> Bool
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
