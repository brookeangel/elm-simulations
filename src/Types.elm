module Types exposing (..)

import Array exposing (Array)


type alias Grid =
    Array (Array CellState)


defaultSize : Int
defaultSize =
    50


type CellState
    = Empty
    | FullOfMoss
    | FullOfTrees


type Rule
    = ChangeFromAToB CellState CellState
