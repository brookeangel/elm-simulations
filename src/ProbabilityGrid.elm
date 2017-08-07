module ProbabilityGrid exposing (ProbabilityRuleGrids, probabilityGridGeneratorForRules)

import Array exposing (Array)
import EveryDict exposing (EveryDict)
import List.Extra
import Random exposing (Generator)
import Types exposing (..)


{-| A collection of probability grids arranged in a dict
-}
type alias ProbabilityRuleGrids a =
    EveryDict a ProbabilityGrid


{-| A probability grid for a single rule
-}
type alias ProbabilityGrid =
    Array (Array Float)


probabilityGridGeneratorForRules : List a -> Generator (ProbabilityRuleGrids a)
probabilityGridGeneratorForRules rules =
    Random.list (List.length rules) probabilityGridGenerator
        |> Random.map (\probabilityGridList -> List.Extra.zip rules probabilityGridList)
        |> Random.map EveryDict.fromList


probabilityGridGenerator : Generator ProbabilityGrid
probabilityGridGenerator =
    Random.list defaultSize
        (Random.list defaultSize (Random.float 0 1)
            |> Random.map Array.fromList
        )
        |> Random.map Array.fromList
