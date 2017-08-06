module Model exposing (..)

import EveryDict exposing (EveryDict)
import Rules exposing (..)
import Types exposing (Grid)


type alias Model =
    { grid : Grid
    , rules : List Rule
    , probabilityGrid : EveryDict Rule ProbabilityGrid
    }
