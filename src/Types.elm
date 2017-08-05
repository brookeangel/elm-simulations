module Types exposing (Cell, CellState(..), Grid, Rule(..), initGrid)

import Array exposing (Array)


type alias Grid =
    Array (Array Cell)


initGrid : Grid
initGrid =
    Array.repeat defaultSize (Array.repeat defaultSize initCell)


defaultSize : Int
defaultSize =
    50


type alias Cell =
    { updated : Bool
    , state : CellState
    }


initCell : Cell
initCell =
    { state = Empty
    , updated = False
    }


type CellState
    = Empty
    | FullOfMoss
    | FullOfTrees


type Rule
    = ChangeFromAToB CellState CellState
