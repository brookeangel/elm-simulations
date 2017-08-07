module Model exposing (..)

import ProbabilityGrid exposing (ProbabilityRuleGrids)
import Rules exposing (..)
import Types exposing (Grid)


type alias Model =
    { grid : Grid
    , rules : List Rule
    , probabilityGrid : ProbabilityRuleGrids Rule
    }
