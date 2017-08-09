module Model exposing (..)

import Random exposing (Seed)
import Rules exposing (..)
import Types exposing (Grid)


type alias Model =
    { grid : Grid
    , rules : List Rule
    , seed : Seed
    }
